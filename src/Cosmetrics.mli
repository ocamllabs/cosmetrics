(** Perform statistics on a set of git repositories. *)

open CalendarLib

module Timeseries : sig
  type 'a t
  (** A time-series for values of type ['a]. *)

  val to_list : 'a t -> (Calendar.t * 'a) list
  (** Return the time-series as an associative list in increasing
      order of the dates. *)

  val dates : 'a t -> Calendar.t list
  (** Return the dates in the time-series in increasing order. *)

  val values : 'a t -> 'a list
  (** [values t] return the values in the time-series in the same
      order as [dates t]. *)

  val map : 'a t -> ('a -> 'b) -> 'b t

  val mapi : 'a t -> (Calendar.t -> 'a -> 'b) -> 'b t

  val iter : 'a t -> (Calendar.t -> 'a -> unit) -> unit

  val fold : 'a t -> f:(Calendar.t -> 'a -> 'b -> 'b) -> 'b -> 'b
  (** [fold t f a] computes [f dN vN (... f d1 v1 init ...)] where
      [d1],...,[dN] are the dates in the time-series in increasing
      order and [v1],...,[vN] are the associated values. *)

  val merge : 'a t -> 'b t ->
              (Calendar.t -> 'a option -> 'b option -> 'c option) -> 'c t

  val sum : float t -> float t -> float t


  val start : _ t -> Calendar.t
  (** The first date in the time-series. *)

  val stop : _ t -> Calendar.t
  (** The last date in the time-series. *)

  val get_exn : 'a t -> Calendar.t -> 'a
  (** [get_exn t d] returns the data associated to a particular date.
      @raise Not_found if the date is not present in the time-series.  *)

  val add : 'a t -> Calendar.t -> 'a -> 'a t
  (** Add a date and associated value to the time-series.  If the date
      already exists, its previous value is replaced. *)

  val empty : 'a t
  ;;
end


(** Commits in Git repositories. *)
module Commit : sig
  type t

  module Set : Set.S with type elt = t

  val date : t -> Calendar.t
  val author : t -> string
  val sha1 : t -> Git.SHA.Commit.t

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool

  val date_range_exn : Set.t -> Calendar.t * Calendar.t
  (** Return the first and last dates (times are erased) of a list of
      commits.
      @raise Invalid_argument if the list is empty. *)

  val timeseries :
    [`Week | `Month] -> ?start: Calendar.t -> ?stop: Calendar.t ->
    Set.t -> int Timeseries.t
  (** [timeseries period commits] returns a list in time-increasing
      order of the number of commits per week (starting on Sunday) or
      per month depending on [period]. *)

  val timeseries_author :
    [`Week | `Month] -> ?start: Calendar.t -> ?stop: Calendar.t ->
    Set.t -> int Timeseries.t
  (** Return a time series of the number of authors contrinuting per
      period of time, regardless of how many commits they made. *)

  val busyness :
    [`Week | `Month] -> ?start: Calendar.t -> ?stop: Calendar.t ->
    ?pencil: float array -> ?offset: int ->
    Set.t -> float Timeseries.t
  (** Return an "busyness" measure (in the interval [0.] … [1.]) of
      the project along time. *)
  ;;
end

module Tag : sig
  type t
  (** Git tag. *)

  val name : t -> string

  val date : t -> Calendar.t

  val cmp_date : t -> t -> int
  (** Compare two tags by increasing date. *)

  val get : Git_unix.FS.t -> t list Lwt.t
  ;;
end


(** DAG of commits. *)
module History : Graph.Sig.P  with type V.t = Commit.t

module StringMap : Map.S  with type key = string

val get_store : ?repo_dir: string -> ?update: bool -> string
                -> Git_unix.FS.t Lwt.t

val commits : ?merge_commits: bool -> Git_unix.FS.t -> Commit.Set.t Lwt.t
(** Return the commits in the history.  Unless [merge_commits] is
    [true], the merge commits are not returned (this is the default). *)

val history : Git_unix.FS.t -> History.t Lwt.t
(** [history remote_uri] returns the DAG representing the history of
    the Git repository at [remote_uri].

    @repo_dir The directory (default ["repo"]) in which a subdirectory
    will be created to hold a copy of the Git history (this is useful
    not to fetch again the repository).  The sudirectory name is based
    on the basename of [remote_uri]. *)

val authors_timeseries :
  ('r * Commit.Set.t) list -> ('r * Commit.t) Timeseries.t StringMap.t
(** [authors_timeseries repo_commits]: given a list of repositories
    names (represented as type ['r]) and commits, return a map that
    gives, for each author, the time-series of its commits with the
    repository information. *)

module Summary : sig
  type t = {
      n: int;     (** number of commits *)
      pct: float; (** percentage of commits (in [0.] .. [100.]. *)
    }

  val make : Commit.Set.t -> (string * t) list

  val make_map : Commit.Set.t -> t StringMap.t
end

module Cache : sig
  type 'a t

  val make : ?log: (string -> unit Lwt.t) ->
             depends: string list -> version: string ->
             update: (unit -> 'a Lwt.t) -> string -> 'a t

  val read : 'a t -> 'a Lwt.t

end
