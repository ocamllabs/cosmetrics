open Lwt
open CalendarLib

let add_stats fh (repo, commits) =
  (* Lwt_io.printlf "Stats for %s" repo >>= fun () -> *)
  let summary = Metrics.summary commits in
  let total = List.fold_left (fun s (_, n) -> s + n) 0 summary in
  Lwt_io.fprintlf fh "<p>Total number of commits: %d</p>\
                      <ul>" total >>= fun () ->
  let total = float total in
  Lwt_list.iter_s (fun (a,n) ->
                   Lwt_io.fprintlf fh "<li>%s: %d (%.1f%%)</li>"
                                   a n (100. *. float n /. total)
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
   ></script>\n\
   <style>\n\
   div.graph {
   float: right;
   margin-right: 2ex;
   width: 50%;
   height: 30ex;
   }
   </style>"

module Bucket = struct
  type t = int * int
  let compare ((y1, w1): t) ((y2, w2): t) =
    let cy = compare y1 y2 in
    if cy = 0 then compare w1 w2 else cy

  let int_of_month =
    let open Calendar in
    function
    | Jan -> 1 | Feb -> 2 | Mar -> 3 | Apr -> 4 | May -> 5 | Jun -> 6
    | Jul -> 7 | Aug -> 8 | Sep -> 9 | Oct -> 10 | Nov -> 11 | Dec -> 12

  let of_commit c :t =
    let d = Metrics.Commit.date c in
    (Calendar.year d, int_of_month(Calendar.month d))

  let to_string (y, w) =
    Printf.sprintf "'%d-%02d-%02d'" y w 1
end

module MW = Map.Make(Bucket)

let graph_no = ref 0

let graph_commits fh (repo, commits) =
  let add_week c m =
    let key = Bucket.of_commit c in
    try MW.add key (MW.find key m + 1) m with _ -> MW.add key 1 m in
  let m = Metrics.History.fold_vertex add_week commits MW.empty in
  let name = Filename.basename repo in
  let name = try Filename.chop_extension name with _ -> name in
  Lwt_io.fprintf fh "<div id='%s' class='graph'></div>\n" name >>= fun () ->
  let x, y = MW.fold (fun key cnt (x, y) ->
                      (Bucket.to_string key :: x, string_of_int cnt :: y)
                     ) m ([], []) in
  let x = String.concat ", " x in
  let y = String.concat ", " y in
  incr graph_no;
  Lwt_io.fprintf fh "<script type='text/javascript'>
                     var chart = c3.generate({
                       bindto: '#%s',
                       data: {
                         x: 'x',
                         columns: [
                           ['x', %s],
                           ['commits', %s],
                         ],
                         types: {
                           commits: 'area'
                         },
                       },
                       axis: {
                         x: {
                           type: 'timeseries',
                           tick: { format: '%%Y-%%m' }
                         }
                       }
                     })\n\
                     </script>" name x y

let main project remotes =
  catch (fun () -> Lwt_unix.mkdir project 0o775)
        (fun _ -> return_unit) >>= fun () ->
  Lwt_unix.chdir project >>= fun () ->
  Lwt_io.printf "Updating repositories... " >>= fun () ->
  Lwt_list.map_p (fun repo ->
                  Metrics.history repo >>= fun commits ->
                  return (repo, commits)
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
                      </head>\n\
                      <body>\n\
                      <h1>Stats for %s</h1>" c3_headers project >>= fun () ->
  let process ((repo, _) as r) =
    Lwt_io.fprintlf fh "<h2 style='clear: both'>%s</h2>" repo >>= fun () ->
    graph_commits fh r >>= fun () ->
    add_stats fh r
  in
  Lwt_list.iter_s process repo_commits >>= fun () ->
  Lwt_io.fprintlf fh "</body>\n</html>"

let rec take n = function
  | [] -> []
  | x :: tl -> if n <= 0 then [] else x :: take (n - 1) tl

let () =
  let repos = Mirage_repo.all in
  (* let repos = take 5 repos in *)
  Lwt_main.run (main "mirage" repos)
