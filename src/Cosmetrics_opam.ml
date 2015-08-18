open Printf
open OpamTypes

let iter_packages ?(repository="default") do_with_opam =
  (* opam-lib 1.3 *)
  (* let state = OpamState.load_state "cosmetrics" *)
  (*                                  OpamStateConfig.(!r.current_switch) in *)
  OpamGlobals.(root_dir := default_opam_dir);
  let state = OpamState.load_state "cosmetrics" in
  let repo = OpamState.find_repository_opt
               state (OpamRepositoryName.of_string repository) in
  let repo = match repo with
    | Some r -> r
    | None ->
       let r = OpamState.sorted_repositories state in
       let r = List.map (fun r -> OpamRepositoryName.to_string r.repo_name) r in
       let r = String.concat ", " r in
       invalid_arg(sprintf "No repository %s, use one of %s" repository r) in
  let packages = OpamRepository.packages_with_prefixes repo in
  let process_package package prefix =
    let opam = OpamState.opam state package in
    let url = lazy(OpamState.url state package) in
    do_with_opam package opam url
  in
  OpamPackage.Map.iter process_package packages

type classify =
  | VCS of pin_option
  | Other of OpamPackage.t * OpamFile.OPAM.t


(* Return the [Some git] where [git] is the Git repository URI guessed
   from [url].  Return [None] otherwise. *)
let guess_git =
  let github = Str.regexp "\\(https?://github\\.com/[^/]+/[^/]+\\)"
  and github_io =
    Str.regexp "https?://\\([A-Za-z0-9]+\\)\\.github\\.io/\\([^/]+\\)"
  and bitbucket =
    Str.regexp "https://bitbucket.org/\\([^/]+\\)/\\([^/]+\\)"
  and erratique = Str.regexp "https?://erratique.ch/software/\\([^/]+\\)"
  in
  fun url ->
  if Str.string_match github url 0 then
    Some (Str.matched_group 1 url ^ ".git")
  else if Str.string_match github_io url 0 then
    let user = Str.matched_group 1 url
    and repo = Str.matched_group 2 url in
    Some ("https://github.com/" ^ user ^ "/" ^ repo)
  else if Str.string_match bitbucket url 0 then
    let user = Str.matched_group 1 url
    and repo = Str.matched_group 2 url in
    Some ("git@bitbucket.org:" ^ user ^ "/" ^ repo ^ ".git")
  else if Str.string_match erratique url 0 then
    let repo = Str.matched_group 1 url in
    Some ("http://erratique.ch/repos/" ^ repo ^ ".git")
  else None


let my_repo = "/tmp/opam/repository/packages"

let write_opam ~name ~version ~git opam =
  let opam_file =
    let version = OpamPackage.Version.to_string version in
    let dir = Filename.(concat (concat my_repo name) (name ^ "." ^ version)) in
    OpamFilename.of_string(Filename.concat dir "opam") in
  let opam_version = OpamVersion.(nopatch current) in
  let opam = OpamFile.OPAM.with_opam_version opam opam_version in
  let opam = OpamFile.OPAM.with_dev_repo opam (Some(Git(git, None))) in
  OpamFile.OPAM.write opam_file opam

let rec find_map_exn l f =
  match l with
  | [] -> raise Not_found
  | x :: tl -> match f x with
               | Some y -> y
               | None -> find_map_exn tl f

let git ?(select=fun _ _ -> true) ?(guess_git=guess_git) () =
  let module S = Cosmetrics.StringMap in
  let pkgs = ref S.empty in
  let guess_vcs pkg opam url =
    if select pkg opam then (
      let name = OpamPackage.(Name.to_string(name pkg)) in
      let version = OpamPackage.version pkg in
      let c = match OpamFile.OPAM.dev_repo opam with
        | Some vcs -> VCS vcs
        | _ ->
           (* No VCS provided, guess. *)
           try
             let git = find_map_exn (OpamFile.OPAM.homepage opam) guess_git in
             write_opam ~name ~version ~git opam;
             VCS(Git(git, None))
           with Not_found ->
                match Lazy.force url with
                | Some url ->
                   (match guess_git (fst(OpamFile.URL.url url)) with
                    | Some git -> write_opam ~name ~version ~git opam;
                                  VCS(Git(git, None))
                    | None -> Other (pkg, opam))
                | None -> Other (pkg, opam)
      in
      try let (version', c') = S.find name !pkgs in
          (* Keep the latest version *)
          if OpamPackage.Version.compare version version' > 0 then
            pkgs := S.add name (version, c) !pkgs
      with Not_found ->
        pkgs := S.add name (version, c) !pkgs
    )
  in
  iter_packages guess_vcs;
  let n_pkgs = S.cardinal !pkgs in
  printf "Total # pkgs: %d\n" n_pkgs;
  let n_git = ref 0
  and n_vcs = ref 0
  and no_email = ref 0 in
  S.iter (fun _ (_, c) ->
          match c with
          | VCS(Git _) -> incr n_git
          | VCS _ -> incr n_vcs
          | Other (pkg, opam) ->
             if OpamFile.OPAM.maintainer opam = [] then
               incr no_email
             else (
               let name = OpamPackage.(Name.to_string(name pkg)) in
               let version = OpamPackage.(Version.to_string (version pkg)) in
               let email = String.concat ", " (OpamFile.OPAM.maintainer opam) in
               eprintf "• No VCS: %s %s → %s\n" name version email
             )
         ) !pkgs;
  printf "# Git: %d\n# other VCS: %d\n# no VCS: %d (including no email: %d)\n"
         !n_git !n_vcs (n_pkgs - !n_git - !n_vcs) !no_email