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

let rec cummulative_loop prev = function
  | [] -> []
  | x :: tl -> let prev = prev +. x in prev :: cummulative_loop prev tl

let cummulative l = cummulative_loop 0. l

let graph html ?(per=`Month) ?(aliveness=true) ~start ~stop repo commits =
  let colors = [0x336600; 0xCC6600] in
  let m = Cosmetrics.Summary.make_map commits in
  let is_occasional c =
    not(is_main_author Cosmetrics.(StringMap.find (Commit.author c) m)) in
  let occasionals = List.filter is_occasional commits in
  let l1 = Cosmetrics.Commit.timeseries per ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries per ~start ~stop occasionals in
  let x = List.map fst l1 in
  let y1 = List.map (fun (_, cnt) -> float cnt) l1 in
  let y2 = List.map (fun (_, cnt) -> float cnt) l2 in
  H.timeseries html ~x [("Total", y1); ("Occasional", y2)] ~colors
               ~ylabel:"# commits";
  H.timeseries html ~x [("∑ total", cummulative y1);
                        ("∑ Occasional", cummulative y2)]
               ~colors ~ylabel:"# commits";
  let l1 = Cosmetrics.Commit.timeseries_author per ~start ~stop commits in
  let l2 = Cosmetrics.Commit.timeseries_author per ~start ~stop occasionals in
  let y1 = List.map (fun (_, cnt) -> float cnt) l1 in
  let y2 = List.map (fun (_, cnt) -> float cnt) l2 in
  H.timeseries html ~x [("Total", y1); ("Occasional", y2)] ~colors
               ~ylabel:"# authors";
  if aliveness then (
    let alv0 = Cosmetrics.Commit.aliveness per ~start ~stop commits in
    let alv = List.map snd alv0 in
    H.timeseries html ~x [("Aliveness", alv)] ~colors;
    alv0
  )
  else []

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


let rec sum_lists_1st_el ls ((sum, tls, all_empty) as acc) =
  match ls with
  | [] -> acc
  | [] :: ls -> (* No more elements on that list, same as = 0., drop it *)
     sum_lists_1st_el ls acc
  | (x :: tl) :: ls ->
     (* We reverse the order of lists in accumulating their tails but
        the addition is commutative so we do not care. *)
     sum_lists_1st_el ls (sum +. x, tl :: tls, false)

(* Sum the lists elementwise. *)
let rec sum_lists ls =
  let s, tls, all_empty = sum_lists_1st_el ls (0., [], true) in
  if all_empty then [s]
  else s :: sum_lists tls


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
         let (d0c, d1c) = commits_date_range_exn commits in
         (date_min d0 d0c , date_max d1 d1c) in
       List.fold_left extremes (commits_date_range_exn commits0) tl
    | [] -> invalid_arg "Empty list of repositories" in

  let repo_commits =
    let shorten repo =
      let repo = Filename.basename repo in
      try Filename.chop_extension repo with _ -> repo in
    List.map (fun (r,c) -> let r = shorten r in (r, r ^ ".html", c))
             repo_commits in
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
                 (* All lists correspond to the same times thanks to
                    [~start] and [~stop]. *)
                 let x = List.map fst (List.hd alvs) in
                 let alv = sum_lists (List.map (fun l -> List.map snd l) alvs) in
                 H.timeseries html ~x [("Aliveness", alv)] ~colors:[0x336600]
                              ~ylabel:"# projects alive"
                )
  >>= fun _ -> return_unit

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 5 repos in *)
  Lwt_main.run (main "mirage" repos)
