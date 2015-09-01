(* Minimal library to generate d3 and c3 graphs by embedding JS code
   in the HTML.  FIXME: should be rewritten for a more decent solution. *)

open Lwt
open CalendarLib
module T = Cosmetrics.Timeseries

let single_quote s =
  let b = Buffer.create (2 + String.length s) in
  Buffer.add_char b '\'';
  for i = 0 to String.length s - 1 do
    let c = String.unsafe_get s i in
    if c = '\'' then Buffer.add_char b '\\';
    Buffer.add_char b c
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

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

type graph_type =
  [`Area | `Area_spline | `Area_step | `Bar | `Line | `Spline | `Step]

let string_of_type = function
  | `Line -> "line"
  | `Bar -> "bar"
  | `Spline -> "spline"
  | `Area -> "area"
  | `Area_spline -> "area-spline"
  | `Step -> "step"
  | `Area_step -> "area-step"

let rec list_make n v =
  if n <= 0 then [] else v :: list_make (n - 1) v

let js_of_float (x: float) =
  if neg_infinity < x && x < infinity then string_of_float x
  else "NaN" (* accepted by c3.js for missing data *)

let id_float (x: float) = x

let graph_gen name html ~axisx ~print_x ~print_y1 ~print_y2
              ?(ty=`Area)
              ?(y2label="") ?tys2 ?(colors2=[]) ?y2min ?y2max ?(y2=[])
              ?(ylabel="") ?tys ?ymin ?ymax ?(ylog=false) ~colors y1 =
  let n1 = List.length y1 in
  let n2 = List.length y2 in
  if List.length colors < n1 then
    invalid_arg(Printf.sprintf "Cosmetrics_html.%s: not enough ~colors" name);
  if List.length colors2 < n2 then
    invalid_arg(Printf.sprintf "Cosmetrics_html.%s: not enough ~colors2" name);
  let types = match tys with
    | Some ty ->
       if List.length ty < n1 then
         invalid_arg(Printf.sprintf "Cosmetrics_html.%s: not enough types\
                                     (tys)" name);
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
  print  html "<script type=\"text/javascript\">\n";
  if ylog then (
    (* Keep the original values for the tooltip. *)
    printf html "var y%d = new Array();\n" html.i;
    List.iteri (fun i (name, _ as y) ->
                printf html "y%d['data%d'] = [\n" html.i i;
                print_y1 html i ~f:id_float y;
                print html "];\n"
               ) y1;
  );
  printf html "var chart%d = c3.generate({
                   bindto: '#cosmetrics%d',
                   data: {
                     x: 'x',
                     columns: [
                       ['x', " html.i html.i;
  print_x html;
  print html "],\n";
  let f = if ylog then log10 else id_float in
  List.iteri (fun i y -> printf html "['data%d', " i;
                       print_y1 html i ~f y;
                       print html "],\n"
             ) y1;
  List.iteri (fun i y -> printf html "['data%d', " (n1 + i);
                       print_y2 html i ~f:id_float y;
                       print html "],\n"
             ) y2;
  print html "],\n\
              names: {\n";
  let print_name i name =
    printf html "data%d: %s,\n" i (single_quote name) in
  List.iteri (fun i (name, _) -> print_name i name) y1;
  List.iteri (fun i (name, _) -> print_name (n1 + i) name) y2;
  print html "},\n\
              types: {\n";
  let print_type i ty = printf html "data%d: '%s',\n" i (string_of_type ty) in
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
                  x: {\n\
                    %s\n
                  },
                  y: {
                    label: %s," axisx (single_quote ylabel);
  (match ymin with Some y -> printf html "min: %g," y
                 | None -> ());
  (match ymax with Some y -> printf html "max: %g," y
                 | None -> ());
  printf html "   },
                  y2: {
                    show: %b,
                    label: %s," (n2 <> 0) (single_quote y2label);
  (match y2min with Some y -> printf html "min: %g," y
                 | None -> ());
  (match y2max with Some y -> printf html "max: %g," y
                 | None -> ());
  printf html "   }
                },\n";
  if ylog then (
    printf html "tooltip: {
                   format: {
                     value: function (value, ratio, id, index) {
                       return (y%d[id][index]); },
                   }
                 }\n" html.i;
  );
  printf html "})\n\
              </script>\n"


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

let print_serie html i ~f t_merged =
  let not_first_el = ref false in
  T.iter t_merged (fun _ v -> if !not_first_el then print html ", ";
                            print html (js_of_float (f v.(i)));
                            not_first_el := true)

let timeseries html ?(xlabel="") ?(ty=`Area)
               ?y2label ?tys2 ?colors2 ?y2min ?y2max ?(y2=[])
               ?ylabel ?tys ?ymin ?ymax ?ylog ~colors ts =
  let n2 = List.length y2 in
  (* Put [y2] in front that that the common case where it is empty is O(1) *)
  let t = merge (y2 @ ts) in
  let x =
    T.mapi t (fun d _ -> "'" ^ Printer.Calendar.sprint "%Y-%m-%d" d ^ "'") in
  let print_x html = print html (String.concat ", " (T.values x)) in
  let print_y1 html i ~f _ = print_serie html (n2 + i) ~f t
  and print_y2 html i ~f _ = print_serie html i ~f t in
  let axisx = Printf.sprintf "type: 'timeseries',
                              label: %s,
                              tick: {
                                format: '%%Y-%%m',
                                fit: true,
                                count: 20,
                              }" (single_quote xlabel) in
  graph_gen "timeseries" html ~axisx ~print_x ~print_y1 ~print_y2
            ~ty ?y2label ?tys2 ?colors2 ?y2min ?y2max ~y2
            ?ylabel ?tys ?ymin ?ymax ?ylog ~colors ts


let print_array html _ ~f (_name, y) =
  if Array.length y > 0 then (
    print html (js_of_float (f y.(0)));
    for i = 1 to Array.length y - 1 do
      print html ", ";
      print html (js_of_float(f y.(i)));
    done
  )

let xy html ?(xlabel="") x  ?(ty=`Line)
       ?y2label ?tys2 ?colors2 ?y2min ?y2max ?y2
       ?ylabel ?tys ?ymin ?ymax ?ylog ~colors y1 =
  let print_x html =
    for i = 0 to Array.length x - 1 do
      print html (js_of_float x.(i));
      print html ", ";
    done in
  let axisx = Printf.sprintf "label: %s,
                              tick: {
                                fit: true,
                              }" (single_quote xlabel) in
  graph_gen "xy" html ~axisx ~print_x
            ~print_y1:print_array
            ~print_y2:print_array
            ~ty ?y2label ?tys2 ?colors2 ?y2min ?y2max ?y2
            ?ylabel ?tys ?ymin ?ymax ?ylog ~colors y1


(** The rows and columns of [m] correspond to the groups.
    [m.(i).(j)] expresses the relationship from group [i] to group [j]. *)
let chord html ?(padding=0.05) ?(width=600) ?(height=600)
          ?inner_radius ?outer_radius
          ?names ?tooltips ~colors (m: float array array) =
  let n = Array.length m in
  if List.length colors < n then
    invalid_arg "Cosmetrics_html.chord: too few colors colors";
  (* See https://github.com/mbostock/d3/wiki/Chord-Layout#chords *)
  html.i <- html.i + 1;
  printf html "<div id=\"cosmetrics%d\" class=\"chord\"></div>\n" html.i;
  printf html "<script type=\"text/javascript\">\n\
               var chord%d = d3.layout.chord()
               .padding(%g)
               .sortSubgroups(d3.descending)
               .matrix([\n" html.i padding;
  for i = 0 to Array.length m - 1 do
    print html "[ ";
    let mi = m.(i) in
    for j = 0 to Array.length mi - 1 do
      printf html "%g, " mi.(j)
    done;
    print html "],\n";
  done;
  print html "]);\n";
  let colors = List.map (fun c -> Printf.sprintf "'#%06X'" c) colors in
  printf html "var fill%d = d3.scale.ordinal().domain(d3.range(%d))
               .range([%s]);\n"
         html.i (List.length colors) (String.concat ", " colors);
  (match names with
   | Some names ->
      if List.length names < n then
        invalid_arg "Cosmetrics_html.chord: too few names";
      let names = List.map single_quote names in
      printf html "var chord_names%d = [ %s ];"
             html.i (String.concat ", " names)
   | None -> ());
  (match tooltips with
   | Some tooltips ->
      if List.length tooltips < n then
        invalid_arg "Cosmetrics_html.chord: too few tooltips";
      let tooltips = List.map single_quote tooltips in
      printf html "var chord_tooltips%d = [ %s ];"
             html.i (String.concat ", " tooltips)
   | None -> ());
  printf html "var svg%d = d3.select('#cosmetrics%d').append('svg')
               .attr('width', %d).attr('height', %d)
               .append('g')
               .attr('transform', 'translate(%d, %d)');\n"
         html.i html.i width height (width / 2) (height / 2);
  let inner_radius = match inner_radius with
    | None -> float(min width height)
              *. (if names <> None then 0.35 else 0.41)
    | Some r -> r in
  let outer_radius = match outer_radius with
    | None -> inner_radius *. 1.1
    | Some r -> r in
  printf html "svg%d.append('g').selectAll('path')
               .data(chord%d.groups)
               .enter().append('path')
               .style('fill', function(d) { return fill%d(d.index); })
               .style('stroke', function(d) { return fill%d(d.index); })
               .attr('d', d3.svg.arc().innerRadius(%g).outerRadius(%g))
               .on('mouseover', fade%d(0.01, true))
               .on('mouseout', fade%d(0.9, false))"
         html.i html.i html.i html.i inner_radius outer_radius html.i html.i;
  if tooltips <> None then
    printf html ".append('title').text(function(d, i) {
                 return chord_tooltips%d[i]; })" html.i;
  print html ";\n";
  printf html "svg%d.append('g').attr('class', 'cosmetrics-chord')
               .selectAll('path')
               .data(chord%d.chords)
               .enter().append('path')
               .attr('d', d3.svg.chord().radius(%g))
               .style('fill', function(d) { return fill%d(d.target.index); })
               .style('opacity', 0.9);\n"
         html.i html.i inner_radius html.i;
  if names <> None then (
    printf html "var chord_ticks%d = svg%d.append('g').selectAll('g')
                 .data(chord%d.groups).enter().append('g');\n"
           html.i html.i html.i;
    printf html
           "chord_ticks%d.append('text')
            .each(function(d) { d.angle = (d.startAngle + d.endAngle)/2; })
            .attr('dy', '.35em')
            .attr('text-anchor',
                  function(d) { return d.angle > Math.PI ? 'end' : null; })
            .attr('transform', function(d) {
                  return 'rotate(' + (d.angle * 180 / Math.PI - 90) + ')'
                         + 'translate(' + (%g * 1.02) + ')'
                         + (d.angle > Math.PI ? 'rotate(180)' : '');  })
            .text(function(d) { return chord_names%d[d.index]; });\n"
           html.i outer_radius html.i;
  );
  printf html "function fade%d(opacity, over) {
                 return function(g, i) {
                   var p = svg%d.selectAll('.cosmetrics-chord path');
                   p.filter(function(d) { return d.source.index != i \
                                              && d.target.index != i; })
                   .transition()
                   .style('opacity', opacity);
                   var q = p.filter(function(d) { return d.source.index == i \
                                                      && d.target.index != i; })
                   .transition();
                   if (over) { q.style('stroke', 'black'); }
                   else {
                     q.style('stroke',
                             function(d) { return fill%d(d.target.index); })
                      .style('opacity', 0.9);
                   };
                 };
               }\n" html.i html.i html.i;
  print html "</script>\n"


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
