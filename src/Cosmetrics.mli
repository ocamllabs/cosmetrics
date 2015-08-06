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

  val group : [`Week | `Month] -> ?start: Date.t -> ?stop: Date.t ->
              t list -> (Date.t * int) list
end

(** DAG of commits. *)
module History : Graph.Sig.P  with type V.t = Commit.t

module StringMap : Map.S  with type key = string

val commits : ?merge_commits: bool -> History.t -> Commit.t list
(** Return the commits in the history.  Unless [merge_commits] is
    [true], the merge commits are not returned (this is the default). *)

val history : ?repo_dir: string -> string -> History.t Lwt.t
(** [history remote_uri] returns the DAG representing the history of
    the Git repository at [remote_uri].

    @repo_dir The directory (default ["repo"]) in which a subdirectory
    will be created to hold a copy of the Git history (this is useful
    not to fetch again the repository).  The sudirectory name is based
    on the basename of [remote_uri]. *)

module Summary : sig
  type t = {
      n: int;     (** number of commits *)
      pct: float; (** percentage of commits (in [0.] .. [100.]. *)
    }

  val make : Commit.t list -> (string * t) list

  val make_map : Commit.t list -> t StringMap.t
end
