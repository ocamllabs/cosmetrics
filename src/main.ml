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

let paths html repo_commits =
  let module S = Cosmetrics.StringMap in
  let num_commits = List.mapi (fun i (_, _, c) -> (i, c)) repo_commits in
  (* Map [a]: author → repo time-series *)
  let update_author repo m c =
    let t_author = try S.find (C.Commit.author c) m
                   with Not_found -> T.empty in
    (* FIXME: although unlikely, one should handle better when 2
       commits happen at the very same time. *)
    let t_author = T.add t_author (C.Commit.date c) repo in
    S.add (C.Commit.author c) t_author m in
  let add_from_repo m (repo, commits) =
    List.fold_left (update_author repo) m commits in
  let a = List.fold_left add_from_repo S.empty num_commits in
  (* Create the matrix *)
  let n = List.length repo_commits in
  let m = Array.create_matrix n n 0. in
  let process_author _ t =
    (* For each transition to another repo, add a link in [m]. *)
    let prev_repo = ref(-1) in (* no such repo *)
    T.iter t (fun _ repo ->
              if !prev_repo < 0 then prev_repo := repo
              else if repo <> !prev_repo then (
                m.(!prev_repo).(repo) <- m.(!prev_repo).(repo) +. 1.;
                prev_repo := repo;
              )
             );
    () in
  S.iter process_author a;
  H.print html "<div class='chord-graph'>";
  let colors = List.mapi (fun i _ -> hue(float i /. float n)) repo_commits in
  let names = List.map (fun (r,_,_) -> r) repo_commits in
  H.chord html m ~colors ~names;
  H.print html "<ul>\n";
  List.iter2 (fun (r,_,_) c ->
              H.printf html "<li style='padding: 4px'
                             ><span style='background-color: #%06X; \
                             padding: 5px'
                             >%s</span></li>" c r
             ) repo_commits colors;
  H.print html "</ul>\n</div>\n"


let date_min d1 d2 =
  if Calendar.compare d1 d2 <= 0 then d1 else d2

let date_max d1 d2 =
  if Calendar.compare d1 d2 >= 0 then d1 else d2

let sum = function
  | [] -> T.empty
  | [t] -> t
  | t0 :: tl -> List.fold_left T.sum t0 tl

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
  let process ?(busyness=true) ?(more=fun _ -> ()) (repo, fname, commits) =
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
                  .main {
                    color: #336600;
                  }";
    add_links html;
    H.printf html "<h1>Stats for %s (project = %s)</h1>" repo project;
    let alv = graph html ~start ~stop repo commits ~busyness in
    more html;
    add_stats html repo commits;
    H.write html fname >>= fun () ->
    return alv
  in
  let all_commits = List.concat (List.map (fun (_,_,c) -> c) repo_commits) in
  Lwt_list.map_p process repo_commits >>= fun alvs ->
  let global_graphs html =
    let alv = sum alvs in
    let n = float(List.length alvs) in
    let alv2 = T.map alv (fun s -> 100. *. s /. n) in
    H.timeseries html [("Busyness", alv)]
                 ~y2:[("% Busyness", alv2)]
                 ~colors:[0x336600] ~colors2:[0x336600]
                 ~ylabel:"# projects busy";
    paths html repo_commits;
  in
  process ("all repositories", "index.html", all_commits)
          ~busyness:false
          ~more:global_graphs
  >>= fun _ -> return_unit

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 10 repos in *)
  Lwt_main.run (main "mirage" repos)
