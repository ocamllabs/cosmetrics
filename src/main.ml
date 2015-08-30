open Lwt
open CalendarLib
module C = Cosmetrics
module T = C.Timeseries
module H = Cosmetrics_html
open Cosmetrics_utils

let is_finite x = neg_infinity < x && x < infinity

let is_main_author s =
  Cosmetrics.Summary.(s.n > 5 && s.pct > 1.)

let add_stats html repo commits =
  let summary = Cosmetrics.Summary.make commits in
  let open Cosmetrics in
  let total = List.fold_left (fun s (_, n) -> s + n.Summary.n) 0 summary in
  H.printf html "<p>Total number of commits (excl. merge): %d</p>\n\
                 <ol>" total;
  List.iter (fun (a,s) ->
             let main = if is_main_author s then "main"
                        else "occasional" in
             H.printf html "<li class='%s'>%s: %d (%.1f%%)</li>"
                       main a s.Summary.n s.Summary.pct
            ) summary;
  H.printf html "</ol>"

let rec cummulative_loop prev = function
  | [] -> []
  | x :: tl -> let prev = prev +. x in prev :: cummulative_loop prev tl

let cummulative =
  let sum d x (prev, t') =
    let prev = prev +. x in
    (prev, T.add t' d prev) in
  fun t -> let _, t' = T.fold t ~f:sum (0., T.empty) in
         t'

let graph html ?(per=`Month) ?(busyness=true) ~start ~stop repo commits =
  let colors = [0x336600; 0xCC6600] in
  let m = Cosmetrics.Summary.make_map commits in
  let is_occasional c =
    not(is_main_author Cosmetrics.(StringMap.find (Commit.author c) m)) in
  let occasionals = C.Commit.Set.filter is_occasional commits in
  let l1 = Cosmetrics.Commit.timeseries per ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries per ~start ~stop occasionals in
  let l1 = T.map l1 float in
  let l2 = T.map l2 float in
  H.timeseries html [("Total", l1); ("Occasional", l2)]
               ~colors ~ylabel:"# commits";
  H.timeseries html [("∑ total", cummulative l1);
                     ("∑ Occasional", cummulative l2)]
               ~colors ~ylabel:"# commits";
  let l1 = Cosmetrics.Commit.timeseries_author per ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries_author per ~start ~stop occasionals in
  let l1 = T.map l1 float in
  let l2 = T.map l2 float in
  H.timeseries html [("Total", l1); ("Occasional", l2)]
               ~colors ~ylabel:"# authors";
  if busyness then (
    let alv0 = Cosmetrics.Commit.busyness per ~start ~stop commits in
    H.timeseries html [("Busyness", alv0)] ~colors;
    alv0
  )
  else T.empty

(* Copied from http://colorbrewer2.org/ *)
let color_scheme = [| 0xa6cee3; 0x1f78b4; 0xb2df8a; 0x33a02c; 0xfb9a99;
                      0xe31a1c; 0xfdbf6f; 0xff7f00; 0xcab2d6; 0x6a3d9a;
                      0xffff99; 0xb15928 |]
let color =
  let n = Array.length color_scheme in
  fun i -> color_scheme.(i mod n)

let sum = function
  | [] -> T.empty
  | [t] -> t
  | t0 :: tl -> List.fold_left T.sum t0 tl


(* Given an array [a] sorted in increasing order and [x] such that
   [a.(i0) <= x < a.(i1)], find [i] such that [a.(i) <= x < a.(i+1)]. *)
let rec index cmp a x i0 i1 =
  let d = i1 - i0 in
  if d < 2 then i0
  else let i = i0 + d / 2 in
       let c = cmp x a.(i) in
       if c < 0 then index cmp a x i0 i
       else (* c >= 0 *) index cmp a x i i1

let create_number_projects starts =
  let starts = Array.of_list(List.sort Calendar.compare starts) in
  let n = Array.length starts in
  fun d ->
  if Calendar.compare d starts.(0) < 0 then 0
  else if Calendar.compare d starts.(n - 1) >= 0 then n
  else index Calendar.compare starts d 0 (n - 1) + 1

let busyness repo_commits busys html =
  let busy = sum busys in
  let start_time c =
    let d = fst(C.Commit.date_range_exn c) in
    (* Match to the start of the chosen period. *)
    (* FIXME: We have to take into account that busyness spread in
         the past.  If we do not want to have a % busyness > 100%, we
         must shift the date. *)
    let d = Calendar.make (Calendar.year d)
                          (Date.int_of_month (Calendar.month d) - 2)
                          1 0 0 0 in
    d in
  let starts = List.map (fun (_,_,c) -> start_time c) repo_commits in
  let number_projects = create_number_projects starts in
  let n_start = T.mapi busy (fun d s -> float(number_projects d)) in
  let busy2 = T.mapi busy (fun d s ->
                           let n = float(number_projects d) in
                           if n = 0. then 0. else 100. *. s /. n) in
  H.timeseries html [("Busyness", busy); ("# projects", n_start)]
               ~y2:[("% Busyness / started projects", busy2)]
               ~colors:[0x336600; 0xC2C2A3] ~tys:[`Area; `Line]
               ~colors2:[0xCC6600] ~tys2:[`Line] ~y2min:0.
               ~ylabel:"# projects" ~y2label:"% projects"


let paths html repo_commits =
  let num_commits = List.mapi (fun i (_, _, c) -> (i, c)) repo_commits in
  let a = Cosmetrics.authors_timeseries num_commits in
  (* Create the matrix *)
  let n = List.length repo_commits in
  let m = Array.make_matrix n n 0. in
  let process_author _ t =
    (* For each transition to another repo, add a link in [m]. *)
    let prev_repo = ref(-1) in (* no such repo *)
    let not_avoid = Array.make n true in
    T.iter t (fun _ (repo, _) ->
              if not_avoid.(repo) then (
                if !prev_repo >= 0 then
                  m.(!prev_repo).(repo) <- m.(!prev_repo).(repo) +. 1.;
                not_avoid.(repo) <- false;
              );
              prev_repo := repo;
             ) in
  let process_author _ t =
    let fst_repo = ref(-1) in
    try T.iter t (fun _ (repo, _) ->
                  if !fst_repo < 0 then fst_repo := repo
                  else if repo <> !fst_repo then (
                    m.(!fst_repo).(repo) <- m.(!fst_repo).(repo) +. 1.;
                    raise Exit
                  )
                 );
        (* We did not find a contribution to another repo, make a
           link from the 1st repo to itself. *)
        if !fst_repo >= 0 then
          m.(!fst_repo).(!fst_repo) <- m.(!fst_repo).(!fst_repo) +. 1.;
    with Exit -> ()
  in
  Cosmetrics.StringMap.iter process_author a;
  H.style html "div.chord-graph {
                  width: 80%;
                }
                div.chord {
                }";
  H.print html "<div class='chord-graph'>";
  let colors = List.mapi (fun i _ -> color i) repo_commits in
  let names = List.map (fun (r,_,_) -> r) repo_commits in
  H.chord html m ~colors ~names ~width:1000 ~height:1000
          ~inner_radius:300.;
  H.print html "</div>\n"

let contribution_order repo_commits fname =
  let html = H.make () in
  let module S = Set.Make(String) in
  let num_commits = List.mapi (fun i (_, _, c) -> (i, c)) repo_commits in
  let a_ts = Cosmetrics.authors_timeseries num_commits in
  (* [c.(i).(j)] is the number of people contributing for the first
     time to the repository number [i] as the [j+1]th repository they
     contribute.  So [c.(0)] gives the number of authors who
     commited to each of the repositories as their first contribution.  *)
  let n = List.length repo_commits in
  let c = Array.make_matrix n n 0 in
  let a = Array.make_matrix n n S.empty in
  let process_author author t =
    let repo_ok = Array.make n true in
    let repo_nth = ref 0 in
    T.iter t (fun _ (repo, _) ->
              if repo_ok.(repo) then (
                c.(!repo_nth).(repo) <- c.(!repo_nth).(repo) + 1;
                a.(!repo_nth).(repo) <- S.add author a.(!repo_nth).(repo);
                incr repo_nth;
                repo_ok.(repo) <- false;
              )
             ) in
  Cosmetrics.StringMap.iter process_author a_ts;
  let n_authors = float(Cosmetrics.StringMap.cardinal a_ts) in
  H.style html ".not-important {
                  color: #939393;
                }";
  let display_table ~nth =
    if nth < n then (
      let c = c.(nth) in
      let a = a.(nth) in
      let repos = List.mapi (fun i (r,_,_) -> r, c.(i), a.(i)) repo_commits in
      let repos =
        List.sort (fun (_,c1,_) (_,c2,_) -> compare (c2:int) c1) repos in
      H.print html "<table class='contribution-order'>";
      H.printf html "<tr><th colspan='2'>Repo #%d contrib</th></tr>" (nth + 1);
      List.iter (fun (r,c,a) ->
                 let title = String.concat ", " (S.elements a) in
                 H.printf html "<tr %s><td>%s</td><td title='%s'\
                                >%d (%.1f%%)</td></tr>\n"
                          (if c = 0 then "class='not-important'" else "")
                          r title c (100. *. float c /. n_authors)
                ) repos;
      H.print html "</table>"
    ) in
  paths html repo_commits;
  H.print html "<table><tr><td>\n";
  display_table ~nth:0;
  H.print html "</td><td>";
  display_table ~nth:1;
  H.print html "</td><td>";
  display_table ~nth:2;
  H.print html "</td></tr></table>\n";
  H.write html fname

(* The tag list is supposed to be sorted by increasing dates. *)
let rec average_periods ~apart ((sum, n) as acc) = function
  | [] | [_] -> sum /. float n
  | d0 :: ((d1 :: _) as tl) ->
     let d0 = Calendar.to_unixfloat(Cosmetrics.Tag.date d0) in
     let d1 = Calendar.to_unixfloat(Cosmetrics.Tag.date d1) in
     let d = d1 -. d0 in
     if d >= apart then average_periods ~apart (sum +. d, n + 1) tl
     else average_periods ~apart acc tl

let one_day = 60. *. 60. *. 24. (* sec *)

let average_releases ?(apart=one_day) remotes fname =
  let process_repo (pkg, remote_uri, _) =
    Cosmetrics.get_store remote_uri >>= fun store ->
    Cosmetrics.Tag.get store >|= fun tags ->
    let tags = List.sort Cosmetrics.Tag.cmp_date tags in
    let avg = average_periods ~apart (0., 0) tags in
    (pkg, tags, avg)
  in
  Lwt_list.map_p process_repo remotes >>= fun repo_average ->
  let repo_average =
    List.sort (fun (_,_,a1) (_,_,a2) -> compare a1 a2) repo_average in
  let html = H.make () in
  H.style html ".average-releases .not-important {
                  color: #939393;
                }";
  H.print html "<h2>Average time between releases</h2>\n";
  H.print html "<table class='average-releases'>\n  \
                <tr><td>Repo</td><td># tags</td><td>Average</td></tr>\n";
  let print (pkg, tags, avg) =
    let days = avg /. 86400. in
    let months = days /. 30. in
    let a = if is_finite months then
              if months < 1. then
                Printf.sprintf "<span class='not-important'>≈ %.0f days</span>"
                               days
              else Printf.sprintf "≈ %.0f months" months
            else "/" in
    let tags = List.map Cosmetrics.Tag.name tags in
    H.printf html "<tr><td>%s</td><td><span title=%s>%d</span></td>\
                   <td>%s</td></tr>\n"
             pkg (H.single_quote (String.concat ", " tags))
             (List.length tags) a in
  List.iter print repo_average;
  H.print html "</table>\n";
  H.write html fname

let date_min d1 d2 =
  if Calendar.compare d1 d2 <= 0 then d1 else d2

let date_max d1 d2 =
  if Calendar.compare d1 d2 >= 0 then d1 else d2



let filter_ocaml_repos repo_commits =
  let classify (p, remote_uri, commits) =
    C.get_store remote_uri >>= fun store ->
    C.classify store >|= fun cl ->
    (p, remote_uri, commits, cl) in
  Lwt_list.map_p classify repo_commits >|= fun r ->
  List.filter_map (function (p,r,c, C.OCaml) -> Some (p,r,c)
                          | _ -> None) r

let main project repo_commits =
  filter_ocaml_repos repo_commits >>= fun repo_commits ->

  let read_cache (p,r,c) =
    let p = OpamPackage.(Name.to_string(name p)) in
    C.Cache.read c >|= fun c -> (p,r,c) in
  Lwt_list.map_p read_cache repo_commits >>= fun repo_commits ->
  let repo_commits =
    List.filter (fun (_,_,c) -> not(C.Commit.Set.is_empty c)) repo_commits in
  Lwt_io.printlf "# repositories used: %d" (List.length repo_commits)
  >>= fun () ->

  let start, stop =
    match repo_commits with
    | (_,_, commits0) :: tl ->
       let extremes (d0,d1) (_,_, commits) =
         let d0c, d1c = C.Commit.date_range_exn commits in
         (date_min d0 d0c , date_max d1 d1c) in
       List.fold_left extremes (C.Commit.date_range_exn commits0) tl
    | [] -> invalid_arg "Empty list of repositories" in

  let repo_commits =
    List.sort (fun (n1,_,_) (n2,_,_) -> String.compare n1 n2) repo_commits in

  let process ?(busyness=true) ?(more_graphs=fun _ -> ())
              ?(more=fun _ -> return_unit) ?fname
              (repo, _, commits) =
    let html = H.make () in
    H.style html "div.graph {
                    float: right;
                    margin-right: 2ex;
                    width: 60%;
                    height: 30ex;
                  }
                  .main {
                    color: #336600;
                  }";
    H.print html "<a href='index.html'>Index</a>";
    H.printf html "<h1>Commits and authors (project = %s)</h1>" project;
    let alv = graph html ~start ~stop repo commits ~busyness in
    more_graphs html;
    add_stats html repo commits;
    more html >>= fun () ->
    let fname = match fname with Some n -> n | None -> repo ^ ".html" in
    H.write html fname >>= fun () ->
    return alv
  in
  let all_commits = List.fold_left C.Commit.Set.union C.Commit.Set.empty
                                   (List.map (fun (_,_,c) -> c) repo_commits) in
  Lwt_list.map_p process repo_commits >>= fun busys ->
  let more =
    [("Commits and authors",
      (fun fname -> process ("all repositories", None, all_commits) ~fname
                          ~busyness:false
                          ~more_graphs:(busyness repo_commits busys)
                  >>= fun _ -> return_unit),
      "All_repositories.html");
     ("Contribution order",
      contribution_order repo_commits, "contribution.html");
     ("Average time between releases",
      average_releases repo_commits, "average-releases.html");
    ] in
  Lwt_list.iter_p (fun (_, f, h) -> f h) more >>= fun () ->
  (* Create the index page *)
  let html = H.make () in
  H.print html "<h1>Global stats</h1>";
  H.print html "<ul>";
  List.iter (fun (t, _, h) -> H.printf html "<li><a href=%s>%s</a></li>"
                                     (H.single_quote h) t) more;
  H.print html "</ul>";
  H.print html "<h1>Repositories</h1>\n\
                <ol>\n";
  let link (repo, _, _) =
    H.printf html "<li><a href=\"%s.html\">%s</a></li>\n" repo repo in
  List.iter link repo_commits;
  H.print html "</ol>\n";
  H.write html "index.html"


let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl


let () =
  let clone = ref false in
  let specs = [
      "--clone", Arg.Set clone,
      " Clone or update the repositories of the selected packages";
    ] in
  let specs = Arg.align specs in
  let usage_msg = "" in
  Arg.parse specs (fun _ -> raise(Arg.Bad "No anomynous arg")) usage_msg;

  let select pkg opam =
    (* OpamPackage.(Name.to_string (name pkg)) = "mybuild" *)
    true
  in
  let repos = Cosmetrics_opam.git ~select () in
  Printf.printf "# repos: %d\n%!" (List.length repos);
  (* let repos = take 10 repos in *)
  let project = "opam-repo" in

  (try Unix.mkdir project 0o775 with _ -> ());
  Unix.chdir project;
  if !clone then (
    let clone (pkg, remote_uri) =
      Lwt_io.printf "Cloning or updating repo %s\n%!"
                    (OpamPackage.to_string pkg) >>= fun () ->
      Lwt_io.(flush stdout) >>= fun () ->
      Cosmetrics.get_store remote_uri >>= fun _ ->
      return_unit in
    Lwt_main.run(Lwt_list.iter_s clone repos);
    exit 0;
  );
  let make_cache (p, remote_uri) =
    (* Update the store version if the type change. *)
    let update_exn () : C.Commit.Set.t Lwt.t =
      Cosmetrics.get_store remote_uri ~update:false >>= fun store ->
      Cosmetrics.commits store in
    let update () =
      catch update_exn
            (fun e -> Printf.printf "  *** %s\n%!" (Printexc.to_string e);
                    return C.Commit.Set.empty) in
    let dir = Filename.basename (Git.Gri.to_string remote_uri) in
    let dir = try Filename.chop_extension dir with _ -> dir in
    let fname = Filename.concat "repo" (dir ^ ".commits") in
    (* FIXME: depend on the Git repo *)
    let depends = [] in
    let cache = C.Cache.make ~depends ~version:"1" ~update fname in
    (p, remote_uri, cache) in
  let repos = List.map make_cache repos in
  Lwt_main.run (main project repos)
