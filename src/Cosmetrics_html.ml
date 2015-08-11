(* Minimal library to generate d3 and c3 graphs by embedding JS code
   in the HTML.  FIXME: should be rewritten for a more decent solution. *)

open Lwt
open CalendarLib
module T = Cosmetrics.Timeseries

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


(* Construct a single time series with an array at each time entry.
   Replace missing data with 0. *)
let merge ts =
  let n = List.length ts in
  let t_merged = ref T.empty in
  let add i d v =
    try (T.get_exn !t_merged d).(i) <- v
    with Not_found ->
         let a = Array.make n 0. in (* date not yet present *)
         a.(i) <- v;
         t_merged := T.add !t_merged d a in
  List.iteri (fun i (_, t) -> T.iter t (add i)) ts;
  !t_merged

let print_serie html i t_merged =
  let not_first_el = ref false in
  T.iter t_merged (fun _ v -> if !not_first_el then print html ", ";
                            print html (string_of_float v.(i));
                            not_first_el := true)

let rec list_make n v =
  if n <= 0 then [] else v :: list_make (n - 1) v

let timeseries html ?(xlabel="") ?(ylabel="") ?(ty=`Area)
               ?tys2 ?(colors2=[]) ?(y2=[]) ?tys ~colors ts =
  let n1 = List.length ts in
  let n2 = List.length y2 in
  if List.length colors < n1 then
    invalid_arg "Cosmetrics_html.timeseries: not enough ~colors";
  if List.length colors2 < n2 then
    invalid_arg "Cosmetrics_html.timeseries: not enough ~colors2";
  let types = match tys with
    | Some ty ->
       if List.length ty < n1 then
         invalid_arg "Cosmetrics_html.timeseries: not enough types (tys)";
       ty
    | None -> list_make n1 ty in
  let types2 = match tys2 with
    | Some ty ->
       if List.length ty < n2 then
         invalid_arg "Cosmetrics_html.timeseries: not enough types (tys2)";
       ty
    | None -> list_make n2 ty in
  (* FIXME: should check that |x| the length of all y's. *)
  html.i <- html.i + 1;
  printf html "<div id=\"cosmetrics%d\" class=\"graph\"></div>\n" html.i;
  (* Put [y2] in front that that the common case where it is empty is O(1) *)
  let t = merge (y2 @ ts) in
  let x = T.mapi t (fun d _ -> "'" ^ Printer.Date.to_string d ^ "'") in
  let x = String.concat ", " (T.values x) in
  printf html "<script type=\"text/javascript\">\n\
                 var chart%d = c3.generate({
                   bindto: '#cosmetrics%d',
                   data: {
                     x: 'x',
                     columns: [
                       ['x', %s],\n" html.i html.i x;
  for i = 0 to n1 - 1 do
    printf html "['data%d', " i;
    print_serie html (n2 + i) t;
    print html "],\n"
  done;
  for i = 0 to n2 - 1 do
    printf html "['data%d', " (n1 + i);
    print_serie html i t;
    print html "],\n"
  done;
  print html "],\n\
              names: {\n";
  let print_name i name =
    printf html "data%d: '%s',\n" i (escape_single_quote name) in
  List.iteri (fun i (name, _) -> print_name i name) ts;
  List.iteri (fun i (name, _) -> print_name (n1 + i) name) y2;
  print html "},\n\
              types: {\n";
  let print_type i ty = printf html "data%d: '%s',\n" i (string_of_types ty) in
  List.iteri print_type types;
  List.iteri (fun i ty -> print_type (n1 + i) ty) types2;
  print html "},\n\
              axes: {";
  for i = 0 to n1 - 1 do printf html "data%d: 'y'," i done;
  for i = 0 to n2 - 1 do printf html "data%d: 'y2'," (n1 + i) done;
  print html "},\n\
              colors: {\n";
  List.iteri (fun i c -> printf html "data%d: '#%06X',\n" i c) colors;
  List.iteri (fun i c -> printf html "data%d: '#%06X',\n" (n1 + i) c) colors2;
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
                  },
                  y2: {
                    show: %b
                  }
                }
              })\n\
              </script>"
         (escape_single_quote xlabel)
         (escape_single_quote ylabel)
         (n2 <> 0)


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
