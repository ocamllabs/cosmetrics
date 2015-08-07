open Lwt
open CalendarLib
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

let graph_commits html ~start ~stop repo commits =
  let colors = [0x336600; 0xCC6600] in
  let m = Cosmetrics.Summary.make_map commits in
  let is_occasional c =
    not(is_main_author Cosmetrics.(StringMap.find (Commit.author c) m)) in
  let occasionals = List.filter is_occasional commits in
  let l1 = Cosmetrics.Commit.timeseries `Month ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries `Month ~start ~stop occasionals in
  let x = List.map fst l1 in
  let y1 = List.map (fun (_, cnt) -> float cnt) l1 in
  let y2 = List.map (fun (_, cnt) -> float cnt) l2 in
  H.timeseries html ~x [("Total", y1); ("Occasional", y2)] ~colors;
  let l1 = Cosmetrics.Commit.timeseries_author `Month ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries_author
             `Month ~start ~stop occasionals in
  let y1 = List.map (fun (_, cnt) -> float cnt) l1 in
  let y2 = List.map (fun (_, cnt) -> float cnt) l2 in
  H.timeseries html ~x [("Total", y1); ("Occasional", y2)] ~colors

let date_min d1 d2 =
  if Date.compare d1 d2 <= 0 then d1 else d2

let date_max d1 d2 =
  if Date.compare d1 d2 >= 0 then d1 else d2

let rec commits_date_range_loop d0 d1 = function
  | [] -> (d0, d1)
  | c :: tl ->
     let d = Calendar.to_date (Cosmetrics.Commit.date c) in
     commits_date_range_loop (date_min d0 d) (date_max d1 d) tl

let commits_date_range_exn = function
  | [] -> invalid_arg "date_range_exn: empty list"
  | c :: tl ->
     let d = Calendar.to_date (Cosmetrics.Commit.date c) in
     commits_date_range_loop d d tl


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
  H.printf html "<h1>Stats for %s</h1>" project;
  let start, stop =
    match repo_commits with
    | (_, commits0) :: tl ->
       let extremes (d0,d1) (_, commits) =
         let (d0c, d1c) = commits_date_range_exn commits in
         (date_min d0 d0c , date_max d1 d1c) in
       List.fold_left extremes (commits_date_range_exn commits0) tl
    | [] -> invalid_arg "Empty list of repositories" in
  let process (repo, commits) =
    H.printf html "<h2 style='clear: both'>%s</h2>" repo;
    graph_commits html ~start ~stop repo commits;
    add_stats html repo commits
  in
  let all_commits = List.concat (List.map snd repo_commits) in
  process ("All_repositories", all_commits);
  List.iter process repo_commits;
  H.write html "index.html"

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 5 repos in *)
  Lwt_main.run (main "mirage" repos)
