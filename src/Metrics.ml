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

type summary = {
    n: int;                     (* total number of commits *)
    authors: (string * int) list; (* assoc list of authors to # of commits *)
  }

(* Similar to "git summary".  To each committer, associate the number
   of commits. *)
let summary h =
  let n = History.nb_vertex h in
  let add_commit c m =
    let a = Commit.author c in
    try MS.add a (MS.find a m + 1) m with Not_found -> MS.add a 1 m in
  let m = History.fold_vertex add_commit h MS.empty in
  let authors = MS.fold (fun a v l -> (a, v) :: l) m [] in
  (* Sort so that more frequent contributors come first. *)
  let authors = List.sort (fun (_,n1) (_,n2) -> compare n2 n1) authors in
  { n; authors }
