(** Perform statistics on a set of git repositories. *)

open CalendarLib

(** Commits in Git repositories. *)
module Commit : sig
  type t

  val date : t -> Calendar.t
  val author : t -> string
  val sha1 : t -> Irmin.Hash.SHA1.t

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

(** DAG of commits. *)
module History : Graph.Sig.P  with type V.t = Commit.t

val history : ?repo_dir: string -> string -> History.t Lwt.t
(** [history remote_uri] returns the DAG representing the history of
    the Git repository at [remote_uri].

    @repo_dir The directory (default ["repo"]) in which a subdirectory
    will be created to hold a copy of the Git history (this is useful
    not to fetch again the repository).  The sudirectory name is based
    on the basename of [remote_uri]. *)

val summary : ?merge_commits: bool -> History.t -> (string * int) list

val group_by_week :
  ?merge_commits: bool -> ?start: Date.t -> ?stop: Date.t ->
  History.t -> (Date.t * int) list
