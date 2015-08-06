(** Perform statistics on a set of git repositories. *)

open Lwt
open CalendarLib

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
end

module History = Graph.Persistent.Digraph.ConcreteBidirectional(Commit)

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

module MS = Map.Make(String)

let is_not_merge h c = History.in_degree h c <= 1

(* Similar to "git summary".  To each committer, associate the number
   of commits. *)
let summary ?(merge_commits=false) h =
  let add_commit c m =
    let a = Commit.author c in
    if merge_commits || is_not_merge h c then
      try MS.add a (MS.find a m + 1) m with Not_found -> MS.add a 1 m
    else m in
  let m = History.fold_vertex add_commit h MS.empty in
  let authors = MS.bindings m in
  (* Sort so that more frequent contributors come first. *)
  List.sort (fun (_,n1) (_,n2) -> compare (n2: int) n1) authors


let day_1 = Date.Period.day (-1)
let day_2 = Date.Period.day (-2)
let day_3 = Date.Period.day (-3)
let day_4 = Date.Period.day (-4)
let day_5 = Date.Period.day (-5)
let day_6 = Date.Period.day (-6)

(* Return a date [d] where the day was changed to be the Sunday of the
   week (i.e. the preceding Sunday). *)
let sunday_of_week d =
  match Date.day_of_week d with
  | Date.Sun -> d
  | Date.Mon -> Date.add d day_1
  | Date.Tue -> Date.add d day_2
  | Date.Wed -> Date.add d day_3
  | Date.Thu -> Date.add d day_4
  | Date.Fri -> Date.add d day_5
  | Date.Sat -> Date.add d day_6

let sunday d = sunday_of_week(Calendar.to_date d)

module MW = Map.Make(Date)

let one_week = Date.Period.week 1

(* Add the date [d] and all subsequent weeks until [date_max]
   (excluded) to [m]. *)
let rec add_all_weeks d date_max m =
  if Date.compare d date_max < 0 then
    let m = if MW.mem d m then m else MW.add d 0 m in
    add_all_weeks (Date.add d one_week) date_max m
  else m

let always_true _ = true

let group_by_week ?(merge_commits=false) ?start ?stop h =
  (* Add the bundary dates to [m], if they are provided. *)
  let after_start, m = match start with
    | Some start -> let start = sunday_of_week start in
                    (fun d -> Date.compare d start >= 0),
                    MW.add start (ref 0) MW.empty (* => date in map *)
    | None -> always_true, MW.empty in
  let before_stop, m = match stop with
    | Some stop -> let stop = sunday_of_week stop in
                   if not(after_start stop) then
                     invalid_arg "Metrics.group_by_week: empty range";
                   (fun d -> Date.compare d stop <= 0), MW.add stop (ref 0) m
    | None -> always_true, m in
  (* Add all (non-merge) commits of the history. *)
  let add_commit c m =
    let d = sunday(Commit.date c) in
    if (merge_commits || is_not_merge h c)
       && after_start d && before_stop d then
      try incr(MW.find d m); m
      with Not_found -> MW.add d (ref 0) m
    else m in
  let m = History.fold_vertex add_commit h m in
  let m = MW.map (fun cnt -> !cnt) m in
  (* Make sure all weeks in the range are present, if needed with a
     count of 0 *)
  let date_min, _ = MW.min_binding m in
  let date_max, _ = MW.max_binding m in
  MW.bindings (add_all_weeks (Date.add date_min one_week) date_max m)
