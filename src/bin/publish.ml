(* TODO:

spec:  Result {
  type: 'version',
  registry: true,
  where: undefined,
  raw: 'melwyn95_dummy@1.0.10',
  name: 'melwyn95_dummy',
  escapedName: 'melwyn95_dummy',
  scope: undefined,
  rawSpec: '1.0.10',
  saveSpec: null,
  fetchSpec: '1.0.10',
  gitRange: undefined,
  gitCommittish: undefined,
  hosted: undefined }

  opts = {
    defaultTag: 'latest',
    // if scoped, restricted by default
    access: spec.scope ? 'restricted' : 'public',
    algorithms: ['sha512'],
    ...opts,
    spec,
  }
  For now just implement `public` later do restricted 

registry:  https://registry.npmjs.org/ (Replace with LIGO registry)

patchManifest

buildMetadata

      method: 'PUT',
      body: metadata,
      ignoreBody: true,

      if faile: patchMetadata 
   
    References:
      https://github.com/npm/cli/blob/latest/workspaces/libnpmpublish/lib/publish.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-registry-fetch/lib/index.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-registry-fetch/lib/default-opts.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-package-arg/lib/npa.js

*)

module SSet = Set.Make(String)
let ignore_dirs 
  = SSet.of_list [ ".ligo" ; "_esy" ; "node_modules" ; "esy.lock" ]

let rec get_all_files : string -> string list Lwt.t = fun file_or_dir ->
  let open Lwt.Syntax in
  let* status = Lwt_unix.stat file_or_dir in
  let* files = match status.st_kind with
    S_REG -> Lwt.return [file_or_dir]
  | S_DIR ->
    if SSet.mem ignore_dirs file_or_dir 
    then Lwt.return [] else 
    let all = Sys.ls_dir file_or_dir in
    let* files = 
    Lwt_list.fold_left_s (fun acc f -> 
      let* fs = get_all_files (Filename.concat file_or_dir f) in
      Lwt.return (acc @ fs)  
    ) [] all in
    Lwt.return files
  | S_LNK -> 
    (* npm ignores symlinks in the tarball *)
    Lwt.return [] 
  | S_CHR 
  | S_BLK
  | S_FIFO
  | S_SOCK -> 
    (* Ignore these types of files as they don't makes sense to include in 
       tarball *)
    Lwt.return []
  in
  Lwt.return files

let tar dir = 
  let open Lwt.Syntax in
  let* files = get_all_files "." in
  let* fd = Lwt_unix.openfile "foo.tar" [ Unix.O_CREAT ; Unix.O_RDWR ] 0x777 in
  let* () = Tar_lwt_unix.Archive.create files fd in
  let* () = Lwt_unix.close fd in
  Lwt.return ()

let publish ~ligo_registry =
  (* get root of the project *)
  let cwd = Unix.getcwd () in
  let () = Lwt_main.run (tar cwd) in
  Ok ("", "")