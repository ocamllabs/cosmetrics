(** Perform statistics on a set of git repositories. *)

open Lwt
module T = Irmin.Task

type t = T.t list

(** [project] is the directory where the Git repository will be cloned. *)
let commits ~project remote_uri =
  let dir = Filename.basename remote_uri in
  let dir = try Filename.chop_extension dir with _ -> dir in
  let root = Filename.concat project dir in
  let store = Irmin.basic (module Irmin_unix.Irmin_git.FS)
                          (module Irmin.Contents.String) in
  let config = Irmin_unix.Irmin_git.config ~root ~bare:true () in
  Irmin.create store config Irmin_unix.task >>= fun t ->
  let upstream = Irmin.remote_uri remote_uri in
  Irmin.pull_exn (t "Updating") upstream `Update >>= fun () ->
  Irmin.history (t "history") >>= fun h ->
  let c = Irmin.History.fold_vertex (fun v l -> v :: l) h [] in
  Lwt_list.map_p (fun c -> Irmin.task_of_head (t "task of head") c) c

module M = Map.Make(String)

type summary = {
    n: int;                     (* total number of commits *)
    authors: (string * int) list; (* assoc list of authors to # of commits *)
  }

(* Similar to "git summary".  To each committer, associate the number
   of commits. *)
let summary (l: t) =
  let n = List.length l in
  let add_commit m c =
    let author = T.owner c in
    try M.add author (M.find author m + 1) m
    with Not_found -> M.add author 1 m in
  let m = List.fold_left add_commit M.empty l in
  let authors = M.fold (fun a v l -> (a, v) :: l) m [] in
  (* Sort so that more frequent contributors come first. *)
  let authors = List.sort (fun (_,n1) (_,n2) -> compare n2 n1) authors in
  { n; authors }
