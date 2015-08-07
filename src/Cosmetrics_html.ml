(* Minimal library to generate d3 and c3 graphs by embedding JS code
   in the HTML.  FIXME: should be rewritten for a more decent solution. *)

open Lwt
open CalendarLib

type html = {
    mutable style: string list;
    mutable body: Buffer.t;
    mutable i: int; (* Just to index things *)
  }

let make () = { style = [];
                body = Buffer.create 4096;
                i = 0 }

let style html s = html.style <- s :: html.style

let print html s = Buffer.add_string html.body s

let printf html = Printf.ksprintf (print html)

let timeseries html ~x ~colors ys =
  if List.length colors < List.length ys then
    invalid_arg "Cosmetrics_html.timeseries: not enough colors";
  (* FIXME: should check that |x| the length of all y's. *)
  html.i <- html.i + 1;
  printf html "<div id=\"cosmetrics%d\" class=\"graph\"></div>\n" html.i;
  let x = List.map (fun d -> "'" ^ Printer.Date.to_string d ^ "'") x in
  let x = String.concat ", " x in
  printf html "<script type=\"text/javascript\">\n\
                 var chart%d = c3.generate({
                   bindto: '#cosmetrics%d',
                   data: {
                     x: 'x',
                     columns: [
                       ['x', %s],\n" html.i html.i x;
  let add_y i (_, y) =
    printf html "['data%d', " i;
    let y = String.concat ", " (List.map string_of_float y) in
    print html y;  print html "],\n" in
  List.iteri add_y ys;
  print html "],\n\
              names: {\n";
  (* FIXME: should escape single quotes: *)
  List.iteri (fun i (name, _) -> printf html "data%d: '%s',\n" i name) ys;
  print html "},\n\
              types: {\n";
  List.iteri (fun i _ -> printf html "data%d: 'area',\n" i) ys;
  print html "},\n";
  print html "colors: {\n";
  List.iteri (fun i c -> printf html "data%d: '#%06X',\n" i c) colors;
  print html "},\n\
              },\n\
                axis: {
                  x: {
                    type: 'timeseries',
                    tick: {
                      format: '%Y-%m',
                      fit: true,
                      count: 20,
                    }
                  }
                }
              })\n\
              </script>"


let write html fname =
  let open Lwt_io in
  open_file fname ~mode:output >>= fun fh ->
  let common_head =
    "<!DOCTYPE html>
     <html xmlns=\"http://www.w3.org/1999/xhtml\">
     <head>
     <meta content=\"text/html; charset=utf-8\" http-equiv=\"Content-Type\"/>
     <!-- Load c3.css -->
     <link href=\"https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.css\"
     rel=\"stylesheet\" type=\"text/css\" />
     <!-- Load d3.js and c3.js -->
     <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js\"
     charset=\"utf-8\"></script>
     <script src=\"https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.js\"
     ></script>
     <style>\n" in
  write fh common_head >>= fun () ->
  Lwt_list.iter_s (fun s -> write fh s >>= fun () -> write_char fh '\n')
                  (List.rev html.style) >>= fun () ->
  write fh "</style>\n</head>\n<body>\n" >>= fun () ->
  write fh (Buffer.contents html.body) >>= fun () ->
  write fh "</body>\n</html>"
