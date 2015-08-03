open Lwt

let add_stats fh (repo, commits) =
  (* Lwt_io.printlf "Stats for %s" repo >>= fun () -> *)
  let summary = Metrics.summary commits in
  Lwt_io.fprintlf fh "<h2>%s</h2>\
                      <p>Total number of commits: %d</p>\
                      <ul>" repo summary.Metrics.n >>= fun () ->
  let total = float summary.Metrics.n in
  Lwt_list.iter_s (fun (a,n) ->
                   Lwt_io.fprintlf fh "<li>%s: %d (%.1f%%)</li>"
                                   a n (100. *. float n /. total)
                  ) summary.Metrics.authors >>= fun () ->
  Lwt_io.fprintlf fh "</ul>"

let main project remotes =
  Lwt_io.printlf "Updating repositories..." >>= fun () ->
  Lwt_list.map_p (fun repo ->
                  Metrics.commits project repo >>= fun commits ->
                  return (repo, commits)
                 ) remotes
  >>= fun repo_commits ->
  Lwt_io.printlf "done" >>= fun () ->

  Lwt_io.(open_file "mirage.html" ~mode: output) >>= fun fh ->
  Lwt_io.fprintlf fh "<!DOCTYPE html>\n\
                      <html xmlns='http://www.w3.org/1999/xhtml'>\n\
                      <head>\
                      <meta content='text/html; charset=utf-8' \
                      http-equiv='Content-Type'/>\
                      </head>\n\
                      <body>\n\
                      <h1>Stats for %s</h1>" project >>= fun () ->
  Lwt_list.iter_s (add_stats fh) repo_commits >>= fun () ->
  Lwt_io.fprintlf fh "</body>\n</html>"


let () =
  Lwt_main.run (main "mirage" Mirage_repo.all)
