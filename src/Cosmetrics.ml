
open Lwt
open CalendarLib
open Cosmetrics_utils

module Store = Git_unix.FS
module G = Git_unix.Sync.Make(Store)

let openfile_pool = Lwt_pool.create 100 (fun () -> return_unit)

let read_commit_exn t sha =
  Store.read_exn t (Git.SHA.of_commit sha) >>= fun v ->
  match v with
  | Git.Value.Commit c -> return c
  | Git.Value.Blob _ | Git.Value.Tag _
  | Git.Value.Tree _ ->
     fail(Failure "Cosmetrics.read_commit_exn: not a commit value")

let read_tree_exn t sha =
  Store.read_exn t (Git.SHA.of_tree sha) >>= fun v ->
  match v with
  | Git.Value.Tree c -> return c
  | Git.Value.Blob _ | Git.Value.Tag _
  | Git.Value.Commit _ ->
     fail(Failure "Cosmetrics.read_tree_exn: not a tree value")


(* Simple cache module. *)
module Cache = struct
  type 'a t = { log: string -> unit Lwt.t;
               depends: string list;
               version: string;    (* user version for the data *)
               update: unit -> 'a Lwt.t;
               fname: string;      (* filename for the cache *)
             }

  let log t fmt =
    Printf.ksprintf t.log fmt

  let open_in fname =
    Lwt_io.open_file ~flags:[Unix.O_RDONLY]
                     ~mode:Lwt_io.Input fname

  let open_out fname =
    Lwt_io.open_file ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
                     ~mode:Lwt_io.Output fname

  let update t =
    t.update () >>= fun v ->
    open_out t.fname >>= fun fh ->
    Lwt_io.write fh t.version >>= fun () ->
    Lwt_io.write_char fh '\n' >>= fun () ->
    Lwt_io.write_value fh v ~flags:[Marshal.Closures] >>= fun () ->
    Lwt_io.close fh >|= fun () ->
    v

  let modif_time fname =
    Lwt_unix.stat fname >|= fun s -> s.Unix.st_mtime

  let is_newer t0 fname =
    modif_time fname >|= fun t -> t0 < t

  (* FIXME: since the read is likely to occur several times, we could
     also supplement the disk cache with an in-memory Weak.t one. *)
  let read_exn t () =
    (* Check version *)
    open_in t.fname >>= fun fh ->
    Lwt_io.read_line fh >>= fun version ->
    (* Check dependencies *)
    modif_time t.fname >>= fun t0 ->
    Lwt_list.exists_s (is_newer t0) t.depends >>= fun newer_dep ->
    if version <> t.version || newer_dep then (
      Lwt_io.close fh >>= fun () ->
      log t "Update cache %s\n" t.fname >>= fun () ->
      update t (* previous handle closed, thus only 1 in use *)
    )
    else (
      Lwt_io.read_value fh >>= fun v ->
      Lwt_io.close fh >|= fun () ->
      v
    )

  let read_exn_pool t () =
    Lwt_pool.use openfile_pool (read_exn t)

  let read t =
    catch (read_exn_pool t)
          (function Unix.Unix_error(Unix.ENOENT, _, _) ->
                    (* Cache does not exist, create one *)
                    log t "Create cache %s\n" t.fname >>= fun () ->
                    Lwt_pool.use openfile_pool (fun () -> update t)
                  | e -> fail e)

  let default_log s =
    Lwt_io.write Lwt_io.stdout s >>= fun() ->
    Lwt_io.flush Lwt_io.stdout

  (** Return a cache.  The cache must be [update]d if any of the
      files in [depend] is newer or if the [version] does not
      coincide. *)
  let make ?(log=default_log) ~depends ~version ~update fname =
    { log;  depends;  version;  update; fname }
end


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
                   ?start ?stop (iter_values: (_ -> unit) -> unit) =
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
  iter_values add_value;
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
      sha1: Git.SHA.Commit.t;
    }

  let date t = t.date
  let author t = t.author
  let sha1 t = t.sha1

  let compare c1 c2 = Git.SHA.Commit.compare c1.sha1 c2.sha1
  let hash c = Git.SHA.Commit.hash c.sha1
  let equal c1 c2 = Git.SHA.Commit.equal c1.sha1 c2.sha1

  module StringSet = Set.Make(String)

  module Set = Set.Make(struct type commit = t
                               type t = commit
                               let compare = compare
                        end)

  let of_git sha1 c =
    let a = Git.Commit.(c.author) in
    let t, _ = Git.User.(a.date) in (* FIXME: Use TZ *)
    let date = Calendar.from_unixfloat (Int64.to_float t) in
    let author = Git.User.(a.name) in
    { date; author; sha1 }


  let date_range_exn s =
    if Set.is_empty s then
      invalid_arg "Cosmetrics.Commit.date_range_exn: empty set"
    else
      let c = Set.choose s in (* will be reevaluated in the loop but easier *)
      let d = date c in
      Set.fold (fun c (d_min, d_max) ->
                let d = date c in
                (date_min d_min d, date_max d_max d)
               ) s (d,d)

  let update_count _ date ~next ~prev ~get_bucket =
    incr (get_bucket date)

  let timeseries offset ?start ?stop commits =
    timeseries_gen offset ~date_of_value:date
                   ~empty_bucket:0  ~update_with_value:update_count
                   ?start ?stop (fun f -> Set.iter f commits)

  let update_authors commit date ~next ~prev ~get_bucket =
    let a = get_bucket date in
    a := StringSet.add (author commit) !a

  let timeseries_author offset ?start ?stop commits =
    let l = timeseries_gen offset ~date_of_value:date
                           ~empty_bucket:StringSet.empty
                           ~update_with_value:update_authors
                           ?start ?stop (fun f -> Set.iter f commits) in
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
                           ?start ?stop (fun f -> Set.iter f commits) in
    Timeseries.map l squash_into_01
end

module Tag = struct
  type t = { r: Git.Reference.t;
             name: string;
             date: Calendar.t;
           }

  let name t = t.name
  let date t = t.date

  let cmp_date t1 t2 =
    Calendar.compare (date t1) (date t2)

  let make r t =
    let s = Git.Reference.to_raw r in
    { r;
      name = String.sub s 10 (String.length s - 10);
      date = Calendar.from_unixfloat (Int64.to_float t) }

  let get_ref t r () =
    if String.starting ~w:"refs/tags/" (Git.Reference.to_raw r) then
      Store.read_reference_exn t r >>= fun sha ->
      Store.read_exn t (Git.SHA.of_commit sha) >|= fun v ->
      match v with
      | Git.Value.Commit c ->
         let t, _ (* FIXME: Use TZ *) = Git.(c.Commit.author.User.date) in
         Some(make r t)
      | Git.Value.Tag t ->
         let t, _ = Git.(t.Tag.tagger.User.date) in
         Some(make r t)
      | Git.Value.Blob _ | Git.Value.Tree _ -> None
    else return_none

  (* Like [get_ref] but wait if too many files are already opened. *)
  let get_ref_pool t r =
    Lwt_pool.use openfile_pool (get_ref t r)

  let get t =
    Store.references t >>= fun r ->
    (* Because of the bug https://github.com/mirage/ocaml-git/issues/124
       remove possible duplicates in the list. *)
    let r = List.sort Git.Reference.compare r in
    let r = List.remove_consecutive_duplicates Git.Reference.equal r in
    Lwt_list.filter_map_p (get_ref_pool t) r
end


module History = Graph.Persistent.Digraph.ConcreteBidirectional(Commit)

let is_not_merge c =
  match Git.Commit.(c.parents) with
  | [] | [_] -> true
  | _ -> false

let rec get_parent_commits ~merge_commits t commit sd =
  let add_parent ((s, dealt) as sd) p_sha =
    if Git.SHA.Commit.Set.mem p_sha dealt then return sd
    else
      let dealt = Git.SHA.Commit.Set.add p_sha dealt in
      read_commit_exn t p_sha >>= fun parent ->
      let s = if merge_commits || is_not_merge parent then
                Commit.Set.add (Commit.of_git p_sha parent) s
              else s in
      get_parent_commits ~merge_commits t parent (s, dealt)
  in
  Lwt_list.fold_left_s add_parent sd Git.Commit.(commit.parents)

let commits ?(merge_commits=false) t =
  Store.read_head t >>= fun head ->
  (match head with
   | Some (Git.Reference.SHA sha) -> return sha
   | Some (Git.Reference.Ref r) -> Store.read_reference_exn t r
   | None -> fail(Failure "Cosmetrics.commits: no head")) >>= fun head ->
  read_commit_exn t head >>= fun commit ->
  let s = Commit.Set.add (Commit.of_git head commit) Commit.Set.empty in
  let dealt = Git.SHA.Commit.Set.(add head empty) in
  get_parent_commits ~merge_commits t commit (s, dealt) >|= fun (s, _) ->
  s

(* Add to the graph [h] all the history leading to [sha] (whose commit
   representation for this library [c] is suppose to be in [h]). *)
let rec add_history_to t h commit c =
  (* FIXME: check *)
  let add_parent h p_sha =
    read_commit_exn t p_sha >>= fun parent ->
    let c_parent = Commit.of_git p_sha parent in
    let h = History.add_vertex h c_parent in
    let h = History.add_edge h c_parent c in
    add_history_to t h parent c_parent
  in
  Lwt_list.fold_left_s add_parent h Git.Commit.(commit.parents)

let history t =
  Store.read_head t >>= fun head ->
  (match head with
   | Some (Git.Reference.SHA sha) -> return sha
   | Some (Git.Reference.Ref r) -> Store.read_reference_exn t r
   | None -> fail(Failure "Cosmetrics.get_history: no head")) >>= fun head ->
  read_commit_exn t head >>= fun commit ->
  let c = Commit.of_git head commit in
  let h = History.add_vertex History.empty c in
  add_history_to t h commit c

let from_github = Str.regexp "https?://github.com/"

let get_store ?(repo_dir="repo") ?(update=false) remote_uri =
  (* Work around HTTPS irmin bug: https://github.com/mirage/irmin/issues/259 *)
  let remote_uri =
    let r = Git.Gri.to_string remote_uri in
    Str.replace_first from_github "git://github.com/" r in
  let dir = Filename.basename remote_uri in
  let dir = try Filename.chop_extension dir with _ -> dir in
  let root = Filename.concat repo_dir dir in
  (if not(Sys.file_exists root) then (
     (* OCaml Git is using too much memory for some repos.
        https://github.com/mirage/ocaml-git/issues/125  Use Git to do the
        initial cloning. *)
     let cmd = "", [| "git"; "clone"; "--no-checkout"; remote_uri; root |] in
     Lwt_process.exec cmd >>= fun st ->
     match st with
     | Unix.WEXITED 0 -> return_unit
     | Unix.WEXITED n -> Lwt_io.printlf "Git %s exit %d" remote_uri n
     | Unix.WSIGNALED n -> Lwt_io.printlf "Git killed by signal %d" n
     | Unix.WSTOPPED n -> Lwt_io.printlf "Git stopped by signal %d" n
   )
   else
     return_unit) >>= fun () ->
  Store.create ~root () >>= fun t ->
  (if update then
     catch (fun () -> G.fetch t (Git.Gri.of_string remote_uri)
                            ~update:true >>= fun r ->
                    match Git_unix.Sync.Result.head_contents r with
                    | Some h -> Store.write_head t h
                    | None -> return_unit)
           (fun e -> Lwt_io.printlf "Fail pull %s: %s"
                                  remote_uri (Printexc.to_string e))
   else
     return_unit)
  >>= fun () ->
  return t


type classification = | OCaml | C
                      | Undecided of string

let is_ml e = Git.Tree.(e.perm = `Normal && String.ending ~w:".ml" e.name)

let is_c e = Git.Tree.(e.perm = `Normal && String.ending ~w:".c" e.name)

let is_src e = Git.Tree.(e.perm = `Dir && (e.name = "src" || e.name = "lib"))

let tree_of_value t = function
  | Git.Value.Blob _ -> fail(Failure "Cosmetrics.tree_of_value: blob")
  | Git.Value.Tag _ -> fail(Failure "Cosmetrics.tree_of_value: tag")
  | Git.Value.Tree tree -> return tree
  | Git.Value.Commit c -> read_tree_exn t c.Git.Commit.tree

let classify_tree t tree =
  if List.exists is_ml tree then return OCaml
  else
    try
      let src = List.find is_src tree in
      Store.read_exn t src.Git.Tree.node >>= fun v ->
      tree_of_value t v >|= fun tree ->
      if List.exists is_ml tree then OCaml
      else if List.exists is_c tree then C
      else Undecided "No .ml nor .c file found"
    with Not_found ->
      return(Undecided "No toplevel .ml nor src/ or lib/ dirs")

let classify t =
  Store.read_reference t Git.Reference.head >>= fun sha ->
  match sha with
  | None -> return(Undecided "No head!")
  | Some sha ->
     Store.read_exn t (Git.SHA.of_commit sha) >>= fun v ->
     catch (fun () -> tree_of_value t v >>= fun tree -> classify_tree t tree)
           (function Failure m -> return(Undecided m)
                   | e -> fail e)

module StringMap = Map.Make(String)

let authors_timeseries repo_commits =
  (* Map [m]: author → repo time-series *)
  let update_author repo c m =
    let t_author = try StringMap.find (Commit.author c) m
                   with Not_found -> Timeseries.empty in
    (* FIXME: although unlikely, one should handle better when 2
       commits happen at the very same time. *)
    let t_author = Timeseries.add t_author (Commit.date c) (repo, c) in
    StringMap.add (Commit.author c) t_author m in
  let add_from_repo m (repo, commits) =
    Commit.Set.fold (update_author repo) commits m in
  List.fold_left add_from_repo StringMap.empty repo_commits



module Summary = struct
  type t = {
      n: int;
      pct: float; (* in the interval [0,100] *)
    }

  (* Similar to "git summary".  To each committer, associate the number
     of commits. *)
  let make_map commits =
    let total = ref 0. in
    let add_commit c m =
      total := !total +. 1.;
      let a = Commit.author c in
      try StringMap.add a (StringMap.find a m + 1) m
      with Not_found -> StringMap.add a 1 m in
    let m = Commit.Set.fold add_commit commits StringMap.empty in
    let pct = 100. /. !total in
    StringMap.map (fun n -> { n;  pct = float n *. pct }) m

  let make commits =
    let authors = StringMap.bindings (make_map commits) in
    (* Sort so that more frequent contributors come first. *)
    List.sort (fun (_,s1) (_,s2) -> compare s2.n s1.n) authors
end
