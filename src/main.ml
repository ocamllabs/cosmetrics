open Lwt
open CalendarLib
module T = Cosmetrics.Timeseries
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

let graph html ?(per=`Month) ?(aliveness=true) ~start ~stop repo commits =
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
  if aliveness then (
    let alv0 = Cosmetrics.Commit.aliveness per ~start ~stop commits in
    H.timeseries html [("Aliveness", alv0)] ~colors;
    alv0
  )
  else T.empty

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
  let process ?(aliveness=true) ?(more=fun _ -> ()) (repo, fname, commits) =
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
    add_links html;
    H.printf html "<h1>Stats for %s (project = %s)</h1>" repo project;
    let alv = graph html ~start ~stop repo commits ~aliveness in
    more html;
    add_stats html repo commits;
    H.write html fname >>= fun () ->
    return alv
  in
  let all_commits = List.concat (List.map (fun (_,_,c) -> c) repo_commits) in
  Lwt_list.map_p process repo_commits >>= fun alvs ->
  process ("all repositories", "index.html", all_commits)
          ~aliveness:false
          ~more:(fun html ->
                 let alv = sum alvs in
                 let n = float(List.length alvs) in
                 let alv2 = T.map alv (fun s -> 100. *. s /. n) in
                 H.timeseries html [("Aliveness", alv)]
                              ~y2:[("% Aliveness", alv2)]
                              ~colors:[0x336600] ~colors2:[0x336600]
                              ~ylabel:"# projects alive";
                )
  >>= fun _ -> return_unit

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 5 repos in *)
  Lwt_main.run (main "mirage" repos)
