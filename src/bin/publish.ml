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

let gzip fname fd =
  let file_size = Int.of_int64_exn (Unix.stat fname).st_size in
  Format.printf "foo.tgz file size after tar: %d" file_size;
  let level = 4 in
  let buffer_len = De.io_buffer_size in
  let time () = Int32.of_float (Unix.gettimeofday ()) in
  let i = De.bigstring_create buffer_len in
  let o = De.bigstring_create buffer_len in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  (* Take care of os *)
  let cfg = Gz.Higher.configuration Gz.Unix time in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0 then 0 else
    let bytes = Bytes.create len in
    Format.printf "- %d %d\n" !p len;
    let rd = Caml.Unix.read fd bytes !p len in
    let len = rd in
    Bigstringaf.blit_from_bytes bytes ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ; len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o ; 
  r

module SSet = Set.Make(String)
let ignore_dirs 
  = SSet.of_list [ ".ligo" ; "_esy" ; "node_modules" ; "esy.lock" ]

let rec get_all_files : string -> string list Lwt.t = fun file_or_dir ->
  let open Lwt.Syntax in
  let* status = Lwt_unix.lstat file_or_dir in
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
  (* let () = List.iter ~f:print_endline files in *)
  Lwt.return files

let tar_gzip_base64 dir = 
  let open Lwt.Syntax in
  let* files = get_all_files "." in

  (* Instead of temp.tar give some useful name Hint: debugging *)
  let fname = Filename.concat Filename.temp_dir_name "temp.tar" in
  print_endline fname;

  (* Think about permission when creating foo.tgz; okay for now *)
  let* fd = Lwt_unix.openfile fname [ Unix.O_CREAT ; Unix.O_RDWR ] 0o666 in
  let* () = Tar_lwt_unix.Archive.create files fd in

  let* () = Lwt_unix.close fd in
  let* fd = Lwt_unix.openfile fname [ Unix.O_RDWR ] 0o666 in

  let buf = gzip fname (Lwt_unix.unix_file_descr fd) in
  let encoded = Base64.encode_exn (Buffer.contents buf) in 

  let* () = Lwt_unix.close fd in

  Lwt.return encoded

let publish ~ligo_registry =
  (* get root of the project *)
  let cwd = Unix.getcwd () in
  let enc = Lwt_main.run (tar_gzip_base64 cwd) in
  print_endline enc;
  Ok ("", "")