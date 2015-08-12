
open Lwt
open CalendarLib

module Timeseries = struct
  module MW = Map.Make(Calendar)

  type 'a t = 'a MW.t
  let to_list = MW.bindings
  let dates t = List.map fst (to_list t)
  let values t = List.map snd (to_list t)
  let map t f = MW.map f t
  let mapi t f = MW.mapi f t
  let iter t f = MW.iter f t
  let fold t ~f init = MW.fold f t init

  let start t = fst(MW.min_binding t)
  let stop t = fst(MW.max_binding t)
  let get_exn t d = MW.find d t
  let add t d v = MW.add d v t
  let empty = MW.empty

  let merge t1 t2 f = MW.merge f t1 t2

  let sum_el d v1 v2 =
    match v1, v2 with
    | Some v1, Some v2 -> Some(v1 +. v2)
    | (Some _ as v), None | None, (Some _ as v) -> v
    | None, None -> None

  let sum t1 t2 = MW.merge sum_el t1 t2

  (* Add the date [d] and all subsequent weeks until [date_max]
     (excluded) to [m]. *)
  let rec add_all_offsets_loop t next_date d date_max ~empty_bucket =
    if Calendar.compare d date_max < 0 then
      let t = if MW.mem d t then t else MW.add d empty_bucket t in
      add_all_offsets_loop t next_date (next_date d) date_max ~empty_bucket
    else t

  let add_all_offsets t next_date ~empty_bucket =
    add_all_offsets_loop t next_date (next_date (start t)) (stop t)
                         ~empty_bucket
end


let day_1 = Calendar.Period.day (-1)
let day_2 = Calendar.Period.day (-2)
let day_3 = Calendar.Period.day (-3)
let day_4 = Calendar.Period.day (-4)
let day_5 = Calendar.Period.day (-5)
let day_6 = Calendar.Period.day (-6)

(* Return a date [d] where the day was changed to be the Sunday of the
   week (i.e. the preceding Sunday). *)
let sunday_of_week d =
  match Calendar.day_of_week d with
  | Calendar.Sun -> d
  | Calendar.Mon -> Calendar.add d day_1
  | Calendar.Tue -> Calendar.add d day_2
  | Calendar.Wed -> Calendar.add d day_3
  | Calendar.Thu -> Calendar.add d day_4
  | Calendar.Fri -> Calendar.add d day_5
  | Calendar.Sat -> Calendar.add d day_6

let first_day_of_month d =
  Calendar.make (Calendar.year d) (Date.int_of_month (Calendar.month d)) 1
                0 0 0

let always_true _ = true

let one_week = Calendar.Period.week 1
let add_one_week d = Calendar.add d one_week
let minus_one_week = Calendar.Period.week (-1)
let sub_one_week d = Calendar.add d minus_one_week

let add_one_month d =
  (* [make] performs the necessary normalization. *)
  Calendar.make (Calendar.year d) (Date.int_of_month (Calendar.month d) + 1)
                (Calendar.day_of_month d) 0 0 0

let sub_one_month d =
  Calendar.make (Calendar.year d) (Date.int_of_month (Calendar.month d) - 1)
                (Calendar.day_of_month d) 0 0 0

let timeseries_gen offset ~date_of_value
                   ~empty_bucket ~update_with_value
                   ?start ?stop values =
  let date_for_period, next, prev = match offset with
    | `Week -> sunday_of_week, add_one_week, sub_one_week
    | `Month -> first_day_of_month, add_one_month, sub_one_month in
  (* Add the boundary dates to [m], if they are provided. *)
  let m = ref Timeseries.empty in
  let after_start = match start with
    | Some start -> let start = date_for_period start in
                    (* Make sure this date is in the map: *)
                    m := Timeseries.add !m start (ref empty_bucket);
                    (fun d -> Calendar.compare d start >= 0)
    | None -> always_true in
  let before_stop = match stop with
    | Some stop -> let stop = date_for_period stop in
                   if not(after_start stop) then
                     invalid_arg "Cosmetrics.*timeseries: empty range";
                   m := Timeseries.add !m stop (ref empty_bucket);
                   (fun d -> Calendar.compare d stop <= 0)
    | None -> always_true in
  let get_bucket date =
    try Timeseries.get_exn !m date
    with Not_found ->
         let bucket = ref empty_bucket in
         if after_start date && before_stop date then
           (* Only add the bucket if within the range. *)
           m := Timeseries.add !m date bucket;
         bucket in
  let add_value v =
    let d = date_for_period(date_of_value v) in
    update_with_value v d ~next ~prev ~get_bucket
  in
  List.iter add_value values;
  let m = Timeseries.map !m (fun cnt -> !cnt) in
  (* Make sure all weeks in the range are present, if needed with a
     count of 0 *)
  Timeseries.add_all_offsets m next ~empty_bucket


let date_min d1 d2 =
  if Calendar.compare d1 d2 <= 0 then d1 else d2

let date_max d1 d2 =
  if Calendar.compare d1 d2 >= 0 then d1 else d2

module Commit = struct
  type t = {
      date: Calendar.t;
      author: string;
      sha1: Irmin.Hash.SHA1.t;
    }

  let date t = t.date
  let author t = t.author
  let sha1 t = t.sha1

  let compare c1 c2 = Irmin.Hash.SHA1.compare c1.sha1 c2.sha1
  let hash c = Irmin.Hash.SHA1.hash c.sha1
  let equal c1 c2 = Irmin.Hash.SHA1.equal c1.sha1 c2.sha1

  let of_head t head =
    Irmin.task_of_head (t "task of head") head >>= fun task ->
    let t = Int64.to_float(Irmin.Task.date task) in
    let date = Calendar.from_unixfloat t in
    let author = Irmin.Task.owner task in
    return { date; author; sha1 = head }

  let rec date_range_loop d_min d_max = function
    | [] -> d_min, d_max
    | c :: tl -> let d = date c in
                 date_range_loop (date_min d_min d) (date_max d_max d) tl

  let date_range_exn = function
    | [] -> invalid_arg "Cosmetrics.Commit.date_range_exn: empty list"
    | c :: tl -> let d = date c in
                 date_range_loop d d tl

  let update_count _ date ~next ~prev ~get_bucket =
    incr (get_bucket date)

  let timeseries offset ?start ?stop commits =
    timeseries_gen offset ~date_of_value:date
                   ~empty_bucket:0  ~update_with_value:update_count
                   ?start ?stop commits

  module StringSet = Set.Make(String)

  let update_authors commit date ~next ~prev ~get_bucket =
    let a = get_bucket date in
    a := StringSet.add (author commit) !a

  let timeseries_author offset ?start ?stop commits =
    let l = timeseries_gen offset ~date_of_value:date
                           ~empty_bucket:StringSet.empty
                           ~update_with_value:update_authors
                           ?start ?stop commits in
    Timeseries.map l (fun a -> StringSet.cardinal a)


  let default_offset = 2
  let default_pencil = [| 0.05; 0.1; 0.2; 0.15; 0.10; 0.05; 0.01 |]

  let update_busyness pencil offset commit date ~next ~prev ~get_bucket =
    let b = get_bucket (date: Calendar.t) in
    b := !b +. pencil.(offset);
    let d = ref date in
    for i = -1 downto -offset do
      d := prev !d;
      let b = get_bucket !d in
      b := !b +. pencil.(offset + i)
    done;
    let d = ref date in (* start again from the date of the commit *)
    for i = 1 to Array.length pencil - offset - 1 do
      d := next !d;
      let b = get_bucket !d in
      b := !b +. pencil.(offset + i)
    done

  let squash_into_01 x =
    if x >= 1. then 1.
    else if x <= 0. then 0.
    else x

  let busyness period ?start ?stop
               ?(pencil=default_pencil) ?(offset=default_offset)
               commits =
    if offset < 0 then
      invalid_arg "Cosmetrics.Commit.busyness: offset < 0";
    let n = Array.length pencil in
    if offset >= n then
      invalid_arg(Printf.sprintf "Cosmetrics.Commit.busyness: offset = %d >= \
                                  %d = length pencil" offset n);
    let l = timeseries_gen period ~date_of_value:date
                           ~empty_bucket:0.
                           ~update_with_value:(update_busyness pencil offset)
                           ?start ?stop commits in
    Timeseries.map l squash_into_01
end

module History = Graph.Persistent.Digraph.ConcreteBidirectional(Commit)

let is_not_merge h c = History.out_degree h c <= 1

let commits ?(merge_commits=false) h =
  let add_commit c l =
    if merge_commits || is_not_merge h c then c :: l
    else l in
  History.fold_vertex add_commit h []

module M = Map.Make(Irmin.Hash.SHA1)

(* Transform the history from Irmin representation to ours. *)
let map_history t h0 =
  (* Collect informations. *)
  let vertices = Irmin.History.fold_vertex (fun n l -> n :: l) h0 [] in
  Lwt_list.map_p (Commit.of_head t) vertices >>= fun vertices ->
  (* Provide an fast access to the vertices. *)
  let m = List.fold_left (fun m c -> M.add c.Commit.sha1 c m) M.empty vertices in
  (* Add vertices & edges to the new graph. *)
  let h = M.fold (fun _ c h -> History.add_vertex h c) m History.empty in
  let add_edge c1 c2 h = History.add_edge h (M.find c1 m) (M.find c2 m) in
  let h = Irmin.History.fold_edges add_edge h0 h in
  return h

let history ?(repo_dir="repo") remote_uri =
  let dir = Filename.basename remote_uri in
  let dir = try Filename.chop_extension dir with _ -> dir in
  let root = Filename.concat repo_dir dir in
  let store = Irmin.basic (module Irmin_unix.Irmin_git.FS)
                          (module Irmin.Contents.String) in
  let config = Irmin_unix.Irmin_git.config ~root ~bare:true () in
  Irmin.create store config Irmin_unix.task >>= fun t ->
  let upstream = Irmin.remote_uri remote_uri in
  catch (fun () -> Irmin.pull_exn (t "Updating") upstream `Update)
        (fun e -> Lwt_io.printlf "Fail pull %s: %s"
                               remote_uri (Printexc.to_string e))
  >>= fun () ->
  Irmin.history (t "history") >>= fun h ->
  map_history t h

module StringMap = Map.Make(String)

module Summary = struct
  type t = {
      n: int;
      pct: float; (* in the interval [0,100] *)
    }

  (* Similar to "git summary".  To each committer, associate the number
     of commits. *)
  let make_map commits =
    let total = ref 0. in
    let add_commit m c =
      total := !total +. 1.;
      let a = Commit.author c in
      try StringMap.add a (StringMap.find a m + 1) m
      with Not_found -> StringMap.add a 1 m in
    let m = List.fold_left add_commit StringMap.empty commits in
    let pct = 100. /. !total in
    StringMap.map (fun n -> { n;  pct = float n *. pct }) m

  let make commits =
    let authors = StringMap.bindings (make_map commits) in
    (* Sort so that more frequent contributors come first. *)
    List.sort (fun (_,s1) (_,s2) -> compare s2.n s1.n) authors
end
