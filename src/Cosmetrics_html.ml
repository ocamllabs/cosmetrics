(* Minimal library to generate d3 and c3 graphs by embedding JS code
   in the HTML.  FIXME: should be rewritten for a more decent solution. *)

open Lwt
open CalendarLib

let escape_single_quote s =
  let n = ref 0 in (* number of quotes *)
  for i = 0 to String.length s - 1 do
    if String.unsafe_get s i = '\'' then incr n
  done;
  if !n = 0 then s
  else (
    let b = Buffer.create (!n + String.length s) in
    for i = 0 to String.length s - 1 do
      let c = String.unsafe_get s i in
      if c = '\'' then Buffer.add_char b '\\';
      Buffer.add_char b c
    done;
    Buffer.contents b
  )

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

let string_of_types = function
  | `Line -> "line"
  | `Area -> "area"
  | `Step -> "step"
  | `Area_step -> "area-step"

let timeseries html ?(xlabel="") ?(ylabel="") ?(ty=`Area) ?tys ~x ~colors ys =
  let nb_of_y = List.length ys in
  if List.length colors < nb_of_y then
    invalid_arg "Cosmetrics_html.timeseries: not enough colors";
  let types = match tys with
    | Some ty ->
       if List.length ty < nb_of_y then
         invalid_arg "Cosmetrics_html.timeseries: not enough types";
       ty
    | None -> List.map (fun _ -> ty) ys in
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
  List.iteri (fun i (name, _) -> printf html "data%d: '%s',\n"
                                      i (escape_single_quote name)) ys;
  print html "},\n\
              types: {\n";
  List.iteri (fun i ty -> printf html "data%d: '%s',\n" i (string_of_types ty))
             types;
  print html "},\n";
  print html "colors: {\n";
  List.iteri (fun i c -> printf html "data%d: '#%06X',\n" i c) colors;
  printf html "},\n\
              },\n\
                axis: {
                  x: {
                    type: 'timeseries',
                    tick: {
                      format: '%%Y-%%m',
                      fit: true,
                      count: 20,
                      label: '%s',
                    }
                  },
                  y: {
                    label: '%s'
                  }
                }
              })\n\
              </script>"
         (escape_single_quote xlabel)
         (escape_single_quote ylabel)


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
