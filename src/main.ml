open Lwt
open CalendarLib
module C = Cosmetrics
module T = C.Timeseries
module H = Cosmetrics_html

let is_main_author s =
  Cosmetrics.Summary.(s.n > 5 && s.pct > 1.)

let add_stats html repo commits =
  let summary = Cosmetrics.Summary.make commits in
  let open Cosmetrics in
  let total = List.fold_left (fun s (_, n) -> s + n.Summary.n) 0 summary in
  H.printf html "<p>Total number of commits (excl. merge): %d</p>\n\
                 <ul>" total;
  List.iter (fun (a,s) ->
             let main = if is_main_author s then "main"
                        else "occasional" in
             H.printf html "<li class='%s'>%s: %d (%.1f%%)</li>"
                       main a s.Summary.n s.Summary.pct
            ) summary;
  H.printf html "</ul>"

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
  let occasionals = List.filter is_occasional commits in
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

let hue h =
  let f, hi = modf (abs_float h *. 6.) in
  let f = truncate (256. *. f) in (* 0. ≤ f < 1.  =>  0 ≤ f ≤ 255 *)
  match mod_float hi 6. with
  | 0. -> 0xFF0000 lor (f lsl 8)
  | 1. -> 0x00FF00 lor ((255 - f) lsl 16)
  | 2. -> 0x00FF00 lor f
  | 3. -> 0x0000FF lor ((255 - f) lsl 8)
  | 4. -> 0x0000FF lor (f lsl 16)
  | 5. -> 0xFF0000 lor (255 - f)
  | _ -> assert false

(* Copied from http://colorbrewer2.org/ *)
let color_scheme = [| 0xa6cee3; 0x1f78b4; 0xb2df8a; 0x33a02c; 0xfb9a99;
                      0xe31a1c; 0xfdbf6f; 0xff7f00; 0xcab2d6; 0x6a3d9a;
                      0xffff99; 0xb15928 |]
let color =
  let n = Array.length color_scheme in
  fun i -> color_scheme.(i mod n)

let paths html repo_commits =
  let num_commits = List.mapi (fun i (_, _, c) -> (i, c)) repo_commits in
  let a = Cosmetrics.authors_timeseries num_commits in
  (* Create the matrix *)
  let n = List.length repo_commits in
  let m = Array.create_matrix n n 0. in
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
             );
    () in
  Cosmetrics.StringMap.iter process_author a;
  H.print html "<div class='chord-graph'>";
  let colors = List.mapi (fun i _ -> color i) repo_commits in
  let names = List.map (fun (r,_,_) -> r) repo_commits in
  H.chord html m ~colors ~names ~width:1000 ~height:1000
          ~inner_radius:300.;
  H.print html "</div>\n"


let contribution_order html repo_commits =
  let num_commits = List.mapi (fun i (_, _, c) -> (i, c)) repo_commits in
  let a = Cosmetrics.authors_timeseries num_commits in
  (* [c.(i).(j)] is the number of people contributing for the first
     time to the repository number [i] as the [j+1]th repository they
     contribute.  So [c.(0)] gives the number of authors who
     commited to each of the repositories as their first contribution.  *)
  let n = List.length repo_commits in
  let c = Array.create_matrix n n 0 in
  let process_author _ t =
    let repo_ok = Array.make n true in
    let repo_nth = ref 0 in
    T.iter t (fun _ (repo, _) ->
              if repo_ok.(repo) then (
                c.(!repo_nth).(repo) <- c.(!repo_nth).(repo) + 1;
                incr repo_nth;
                repo_ok.(repo) <- false;
              )
             );
    () in
  Cosmetrics.StringMap.iter process_author a;
  let n_authors = float(Cosmetrics.StringMap.cardinal a) in
  let display_table ~nth =
    let c = c.(nth) in
    let repos = List.mapi (fun i (r,_,_) -> r, c.(i)) repo_commits in
    let repos = List.sort (fun (_, c1) (_, c2) -> compare (c2:int) c1) repos in
    H.print html "<table class='contribution-order'>";
    H.printf html "<tr><th colspan='2'>Repo #%d contrib</th></tr>" (nth + 1);
    List.iter (fun (r,c) ->
               H.printf html "<tr %s><td>%s</td><td>%d (%.1f%%)</td></tr>\n"
                        (if c = 0 then "class='not-important'" else "")
                        r c (100. *. float c /. n_authors)
              ) repos;
    H.print html "</table>" in
  H.print html "<table><tr><td>\n";
  display_table ~nth:0;
  H.print html "</td><td>";
  display_table ~nth:1;
  H.print html "</td><td>";
  display_table ~nth:2;
  H.print html "</td></tr></table>\n"


let date_min d1 d2 =
  if Calendar.compare d1 d2 <= 0 then d1 else d2

let date_max d1 d2 =
  if Calendar.compare d1 d2 >= 0 then d1 else d2

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

let main project remotes =
  catch (fun () -> Lwt_unix.mkdir project 0o775)
        (fun _ -> return_unit) >>= fun () ->
  Lwt_unix.chdir project >>= fun () ->
  Lwt_io.printf "Updating repositories... " >>= fun () ->
  Lwt_list.map_p (fun repo ->
                  Cosmetrics.history repo >>= fun commits ->
                  return (repo, Cosmetrics.commits commits)
                 ) remotes
  >>= fun repo_commits ->
  Lwt_io.printlf "done.%!" >>= fun () ->

  let start, stop =
    match repo_commits with
    | (_, commits0) :: tl ->
       let extremes (d0,d1) (_, commits) =
         let d0c, d1c = Cosmetrics.Commit.date_range_exn commits in
         (date_min d0 d0c , date_max d1 d1c) in
       List.fold_left extremes (Cosmetrics.Commit.date_range_exn commits0) tl
    | [] -> invalid_arg "Empty list of repositories" in

  let repo_commits =
    let shorten repo =
      let repo = Filename.basename repo in
      try Filename.chop_extension repo with _ -> repo in
    List.map (fun (r,c) -> let r = shorten r in (r, r ^ ".html", c))
             repo_commits in
  let repo_commits =
    List.sort (fun (n1,_,_) (n2,_,_) -> String.compare n1 n2) repo_commits in

  let add_links html =
    H.print html "<a href=\"index.html\">All</a>\n";
    let link (repo, fname, _) =
      H.printf html "<a href=\"%s\">%s</a>\n" fname repo in
    List.iter link repo_commits in
  let process ?(busyness=true) ?(more_graphs=fun _ -> ()) ?(more=fun _ -> ())
              (repo, fname, commits) =
    let html = H.make () in
    H.style html "div.graph {
                  float: right;
                    margin-right: 2ex;
                    width: 60%;
                    height: 30ex;
                  }
                  div.chord-graph {
                    float: right;
                    width: 70%;
                  }
                  div.chord {
                    float: right;
                  }
                  .not-important {
                    color: #939393;
                  }
                  .main {
                    color: #336600;
                  }";
    add_links html;
    H.printf html "<h1>Stats for %s (project = %s)</h1>" repo project;
    let alv = graph html ~start ~stop repo commits ~busyness in
    more_graphs html;
    add_stats html repo commits;
    more html;
    H.write html fname >>= fun () ->
    return alv
  in
  let all_commits = List.concat (List.map (fun (_,_,c) -> c) repo_commits) in
  Lwt_list.map_p process repo_commits >>= fun busys ->
  let global_graphs html =
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
                 ~colors2:[0xCC6600] ~tys2:[`Line]
                 ~ylabel:"# projects busy";
    paths html repo_commits;
  in
  let global_tables html =
    contribution_order html repo_commits;
  in
  process ("all repositories", "index.html", all_commits)
          ~busyness:false
          ~more_graphs:global_graphs
          ~more:global_tables
  >>= fun _ -> return_unit

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 10 repos in *)
  Lwt_main.run (main "mirage" repos)
