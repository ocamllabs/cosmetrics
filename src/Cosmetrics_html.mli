(** Minimal library to generate d3 and c3 graphs by embedding JS code
    in the HTML. *)

(** {2 Basic HTML document} *)

type html

val make : unit -> html

val style : html -> string -> unit

val print : html -> string -> unit

val printf : html -> ('a, unit, string, unit) format4 -> 'a

val write : html -> string -> unit Lwt.t
(** [write html fname] write the [html] to the file named [fname]. *)

val single_quote : string -> string
(** [single_quote s] return [s] between single quotes (as used by
    JavaScript), escaping [s] as needed. *)


(** {2 Graphs} *)

type graph_type =
  [`Area | `Area_spline | `Area_step | `Bar | `Line | `Spline | `Step]

val timeseries :
  html ->
  ?xlabel:string ->
  ?ty:graph_type ->
  ?y2label:string -> ?tys2:graph_type list -> ?colors2:int list ->
  ?y2min:float -> ?y2max:float ->
  ?y2:(string * float Cosmetrics.Timeseries.t) list ->
  ?ylabel:string -> ?tys:graph_type list ->
  ?ymin:float -> ?ymax:float -> ?ylog:bool ->
  colors:int list -> (string * float Cosmetrics.Timeseries.t) list -> unit
(** [timeseries ~colors ts] graph the list of timeseries [ts] each
    with the color at the same position in [colors]. *)

val xy :
  html ->
  ?xlabel:string -> float array ->
  ?ty:graph_type ->
  ?y2label:string -> ?tys2:graph_type list -> ?colors2:int list ->
  ?y2min:float -> ?y2max:float -> ?y2:(string * float array) list ->
  ?ylabel:string -> ?tys:graph_type list ->
  ?ymin:float -> ?ymax:float -> ?ylog:bool ->
  colors:int list -> (string * float array) list -> unit

val chord :
  html ->
  ?padding:float -> ?width:int -> ?height:int ->
  ?inner_radius:float -> ?outer_radius:float -> ?names:string list ->
  ?tooltips:string list ->
  colors:int list -> float array array -> unit
