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
    Some ("https://github.com/" ^ user ^ "/" ^ repo ^ ".git")
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

let read_whole_file fname =
  let fh = open_in fname in
  let n = in_channel_length fh in
  let b = Bytes.create n in
  if input fh b 0 n <> n then failwith "read_whole_file";
  close_in fh;
  Bytes.unsafe_to_string b

let version_re = Str.regexp "^opam-version *: * \"[^\"]+\""
let build_re = Str.regexp "^build:"

(* OpamFile.OPAM.write rewrites too much of the file making the actual
   changes in the commit difficult to appreciate.  Use regexpes
   instead. *)
let write_opam ~name ~version ~git opam =
  let version = OpamPackage.Version.to_string version in
  try
    let opam_file =
      let dir = Filename.concat my_repo name in
      let dir = Filename.concat dir (name ^ "." ^ version) in
      Filename.concat dir "opam" in
    let s = read_whole_file opam_file in
    let s = Str.global_replace version_re "opam-version: \"1.2\"" s in
    let dev_repo = sprintf "dev-repo: \"%s\"\nbuild:" git in
    let s = Str.global_replace build_re dev_repo s in
    let fh = open_out opam_file in
    output fh s 0 (String.length s);
    close_out fh
  with e ->
    eprintf "E: %s %s: %s\n" name version (Printexc.to_string e)

let write_opam ~name ~version ~git opam = ()

let rec find_map_exn l f =
  match l with
  | [] -> raise Not_found
  | x :: tl -> match f x with
               | Some y -> y
               | None -> find_map_exn tl f

let git ?(verbose=false) ?(select=fun _ _ -> true) ?(guess_git=guess_git) () =
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
      try let (pkg', c') = S.find name !pkgs in
          (* Keep the latest version *)
          let version' = OpamPackage.version pkg' in
          if OpamPackage.Version.compare version version' > 0 then
            pkgs := S.add name (pkg, c) !pkgs
      with Not_found ->
        pkgs := S.add name (pkg, c) !pkgs
    )
  in
  iter_packages guess_vcs;
  if verbose then (
    let n_pkgs = S.cardinal !pkgs in
    printf "Total # pkgs: %d\n" n_pkgs;
    let n_git = ref 0
    and n_vcs = ref 0
    and no_email = ref 0 in
    let count _ (_, c) =
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
         ) in
    S.iter count !pkgs;
    printf "# Git: %d\n# other VCS: %d\n\
            # no VCS: %d (including no email: %d)\n%!"
           !n_git !n_vcs (n_pkgs - !n_git - !n_vcs) !no_email;
  );
  let pkgs = S.filter (fun _  (_, c) -> match c with VCS(Git _) -> true
                                                 | _ -> false) !pkgs in
  S.fold (fun _ x l -> match x with
                     | (pkg, VCS(Git (g, _))) -> (pkg, g) :: l
                     | _ -> assert false) pkgs []
