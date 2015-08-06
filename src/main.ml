open Lwt
open CalendarLib

let is_main_author s =
  Cosmetrics.Summary.(s.n > 5 && s.pct > 5.)

let add_stats fh repo commits =
  let summary = Cosmetrics.Summary.make commits in
  let open Cosmetrics in
  let total = List.fold_left (fun s (_, n) -> s + n.Summary.n) 0 summary in
  Lwt_io.fprintlf fh "<p>Total number of commits (excl. merge): %d</p>\
                      <ul>" total >>= fun () ->
  Lwt_list.iter_s (fun (a,s) ->
                   let main = if is_main_author s then "main"
                              else "occasional" in
                   Lwt_io.fprintlf fh "<li class='%s'>%s: %d (%.1f%%)</li>"
                                   main a s.Summary.n s.Summary.pct
                  ) summary >>= fun () ->
  Lwt_io.fprintlf fh "</ul>"

let c3_headers =
  "<!-- Load c3.css -->\n\
   <link href=\"https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.css\" \
   rel=\"stylesheet\" type=\"text/css\">\n\
   <!-- Load d3.js and c3.js -->\n\
   <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js\" \
   charset=\"utf-8\"></script>\n\
   <script src=\"https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.js\" \
   ></script>\n"

let string_of_groups g =
  let add_date (x,y) (date, cnt) =
    (("'" ^ Printer.Date.to_string date ^ "'") :: x,
     string_of_int cnt :: y) in
  (* The dates will be in reverse order but c3 does not care. *)
  let x, y = List.fold_left add_date ([], []) g in
  (String.concat ", " x,  String.concat ", " y)

let graph_commits fh ~start ~stop repo commits =
  let m = Cosmetrics.Summary.make_map commits in
  let is_occasional c =
    not(is_main_author Cosmetrics.(StringMap.find (Commit.author c) m)) in
  let occasionals = List.filter is_occasional commits in
  let l1 = Cosmetrics.group_by_week ~start ~stop commits in
  let l2 = Cosmetrics.group_by_week ~start ~stop occasionals in
  let x, y1 = string_of_groups l1 in
  let _, y2 = string_of_groups l2 in
  let name = Filename.basename repo in
  let name = try Filename.chop_extension name with _ -> name in
  Lwt_io.fprintf fh "<div id='%s' class='graph'></div>\n" name >>= fun () ->
  Lwt_io.fprintf fh "<script type='text/javascript'>
                     var chart = c3.generate({
                       bindto: '#%s',
                       data: {
                         x: 'x',
                         columns: [
                           ['x', %s],
                           ['total', %s],
                           ['occasional', %s],
                         ],
                         types: {
                           total: 'area',
                           occasional: 'area'
                         },
                         colors: {
                           total: '#336600',
                           occasional: '#CC6600',
                         }
                       },
                       axis: {
                         x: {
                           type: 'timeseries',
                           tick: {
                             format: '%%Y-%%m',
                             fit: true,
                             count: 20,
                           }
                         }
                       }
                     })\n\
                     </script>" name x y1 y2

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

  Lwt_io.(open_file "index.html" ~mode: output) >>= fun fh ->
  Lwt_io.fprintlf fh "<!DOCTYPE html>\n\
                      <html xmlns='http://www.w3.org/1999/xhtml'>\n\
                      <head>\
                      <meta content='text/html; charset=utf-8' \
                      http-equiv='Content-Type'/>\n\
                      %s\n\
                      <style>\n\
                      div.graph {
                        float: right;
                        margin-right: 2ex;
                        width: 60%%;
                        height: 30ex;
                      }
                      .main {
                        color: #336600;
                      }
                      </style>
                      </head>\n\
                      <body>\n\
                      <h1>Stats for %s</h1>" c3_headers project >>= fun () ->
  let start, stop =
    match repo_commits with
    | (_, commits0) :: tl ->
       let extremes (d0,d1) (_, commits) =
         let (d0c, d1c) = commits_date_range_exn commits in
         (date_min d0 d0c , date_max d1 d1c) in
       List.fold_left extremes (commits_date_range_exn commits0) tl
    | [] -> invalid_arg "Empty list of repositories" in
  let process (repo, commits) =
    Lwt_io.fprintlf fh "<h2 style='clear: both'>%s</h2>" repo >>= fun () ->
    graph_commits fh ~start ~stop repo commits >>= fun () ->
    add_stats fh repo commits
  in
  let all_commits = List.concat (List.map snd repo_commits) in
  process ("All_repositories", all_commits) >>= fun () ->
  Lwt_list.iter_s process repo_commits >>= fun () ->
  Lwt_io.fprintlf fh "</body>\n</html>"

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 5 repos in *)
  Lwt_main.run (main "mirage" repos)
