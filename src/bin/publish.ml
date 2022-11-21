(* 


- [x] Add --dry-run CLI flag to ligo publish
- [x] when directory field is null don't send the key
- [x] Stat main file before publish
- [ ] Implement --dry-run flag
- [ ] Add stats about packed size, upacked size, total files in json
- [ ] Show tarball contents (number of files) & tarball details in CLI output (name, version, filenam[tarball], packed size, unpacked size, shasum, integrity, total files)
- [ ] Wrap logging message in a function ~before ~after
- [ ] Add CLI option to override path to .ligorc
- [ ] Add unit tests for manifest parsing & validation
- [ ] Add expect tests for ligo publish --dry-run which check for valid storage_fn, storage_arg, main
- [ ] 2 tests for tar-gzip (< 1 MB & > 1 MB)
- [ ] Docs: Update docs related to recent changes to package.json (Docs: manifest file)
- [ ] Docs: Add note about #import/include"<pkg>/<path>"
- [ ] Docs: Add note about `--registry`

*)

module LigoRC = Cli_helpers.LigoRC
module LigoManifest = Cli_helpers.LigoManifest
module RepositoryUrl = Cli_helpers.RepositoryUrl
module SMap = Caml.Map.Make (String)

type object_ = (string * string) list

let object__to_yojson o =
  `Assoc (List.fold o ~init:[] ~f:(fun kvs (k, v) -> (k, `String v) :: kvs))


type sem_ver = string [@@deriving to_yojson]
type dist_tag = { latest : sem_ver } [@@deriving to_yojson]

module FilesInfo = struct
  type t =
    { file_count : int
    ; unpacked_size : int
    ; tarball_content : bytes
    ; sha1 : string
    ; sha512 : string
    }

  let make ~size ~fcount ~sha1 ~sha512 ~tarball =
    { file_count = fcount
    ; unpacked_size = size
    ; sha1
    ; sha512
    ; tarball_content = tarball
    }
end

module Dist = struct
  type t =
    { integrity : string
    ; shasum : string
    ; tarball : string
    ; file_count : int [@key "fileCount"]
    ; unpacked_size : int [@key "unpackedSize"]
    }
  [@@deriving to_yojson]

  let make ~tarball ~files_info =
    let FilesInfo.{ sha1; sha512; file_count; unpacked_size; _ } = files_info in
    { integrity = Format.sprintf "sha512-%s" (Base64.encode_exn sha512)
    ; shasum = sha1
    ; tarball
    ; file_count
    ; unpacked_size
    }
end

type author = { name : string } [@@deriving to_yojson]

module Version = struct
  type t =
    { name : string
    ; author : author
    ; main : string option
    ; type_ : string [@key "type"]
    ; storage_fn : string option
    ; storage_arg : string option
    ; repository : RepositoryUrl.t
    ; version : sem_ver
    ; description : string
    ; scripts : object_
    ; dependencies : object_
    ; dev_dependencies : object_ [@key "devDependencies"]
    ; readme : string
    ; bugs : LigoManifest.Bugs.t
    ; id : string [@key "_id"]
    ; dist : Dist.t
    }
  [@@deriving to_yojson]

  let make ~ligo_registry ~version ~files_info ~manifest =
    let LigoManifest.
          { name
          ; version
          ; main
          ; scripts
          ; dependencies
          ; dev_dependencies
          ; description
          ; readme
          ; author
          ; type_
          ; repository
          ; storage_fn
          ; storage_arg
          ; bugs
          ; _
          }
      =
      manifest
    in
    let tarball =
      Format.sprintf "%s/%s/-/%s-%s.tgz" ligo_registry name name version
    in
    { main
    ; name
    ; author = { name = author }
    ; type_
    ; storage_fn
    ; storage_arg
    ; repository
    ; version
    ; description
    ; scripts
    ; dependencies
    ; dev_dependencies
    ; readme
    ; bugs
    ; id = Format.sprintf "%s@%s" name version
    ; dist = Dist.make ~tarball ~files_info
    }
end

module Versions = struct
  type t = Version.t SMap.t

  let to_yojson t =
    let kvs = SMap.fold (fun k v xs -> (k, Version.to_yojson v) :: xs) t [] in
    `Assoc kvs
end

module Attachment = struct
  type t =
    { content_type : string
    ; data : string
    ; length : int
    }
  [@@deriving to_yojson]

  let make ~gzipped_tarball ~size =
    { content_type = "application/octet-stream"
    ; data = Base64.encode_exn (Bytes.to_string gzipped_tarball)
    ; length = size
    }
end

module Attachments = struct
  type t = Attachment.t SMap.t

  let to_yojson t =
    let kvs =
      SMap.fold (fun k v xs -> (k, Attachment.to_yojson v) :: xs) t []
    in
    `Assoc kvs
end

module Body = struct
  type t =
    { id : string [@key "_id"]
    ; name : string
    ; description : string
    ; dist_tags : dist_tag [@key "dist-tags"]
    ; versions : Versions.t
    ; readme : string
    ; attachments : Attachments.t [@key "_attachments"]
    }
  [@@deriving to_yojson]

  let make ~ligo_registry ~files_info ~manifest =
    let LigoManifest.
          { name
          ; version
          ; main
          ; scripts
          ; dependencies
          ; dev_dependencies
          ; description
          ; readme
          ; author
          ; type_
          ; repository
          ; storage_fn
          ; storage_arg
          ; bugs
          ; _
          }
      =
      manifest
    in
    let gzipped_tarball = files_info.FilesInfo.tarball_content in
    let v = Version.make ~ligo_registry ~version ~files_info ~manifest in
    let versions = SMap.add version v SMap.empty in
    { id = name
    ; name
    ; description
    ; dist_tags = { latest = version }
    ; versions
    ; readme
    ; attachments =
        SMap.add
          (Format.sprintf "%s-%s.tgz" name version)
          Attachment.
            { content_type = "application/octet-stream"
            ; data = Base64.encode_exn (Bytes.to_string gzipped_tarball)
            ; length = Bytes.length gzipped_tarball
            }
          SMap.empty
    }
end

let http ~ligo_registry ~manifest ~body ~token =
  let open Cohttp_lwt_unix in
  let LigoManifest.{ name } = manifest in
  let uri = Uri.of_string (Format.sprintf "%s/%s" ligo_registry name) in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "publish"
      ; "authorization", Format.sprintf "Bearer %s" token
      ; "content-type", "application/json"
      ]
  in
  let body =
    body |> Body.to_yojson |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let r = Client.put ~headers ~body uri in
  Lwt.map (fun r -> Ok r) r


let os_type =
  match Sys.os_type with
  | "Unix" -> Gz.Unix
  | "Win32" -> Gz.NTFS
  | "Cygwin" -> Gz.NTFS
  | _ -> Gz.Unix


let gzip fname =
  let fd = Caml_unix.openfile fname [ Core_unix.O_RDWR ] 0o666 in
  let file_size = Int.of_int64_exn (Core_unix.stat fname).st_size in
  let level = 4 in
  let buffer_len = De.io_buffer_size in
  let time () = Int32.of_float (Core_unix.gettimeofday ()) in
  let i = De.bigstring_create buffer_len in
  let o = De.bigstring_create buffer_len in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let cfg = Gz.Higher.configuration os_type time in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0
    then 0
    else (
      let bytes = Bytes.create len in
      let len = Caml_unix.read fd bytes 0 len in
      Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
      p := !p + len;
      len)
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o;
  let () = Caml_unix.close fd in
  r


(* TODO: More files here ?? *)
module SSet = Set.Make (String)

let ignore_dirs =
  SSet.of_list [ ".ligo"; "_esy"; "node_modules"; "esy.lock"; ".git" ]


let rec get_all_files : string -> (string * int) list Lwt.t =
 fun file_or_dir ->
  let open Lwt.Syntax in
  let* status = Lwt_unix.lstat file_or_dir in
  let* files =
    match status.st_kind with
    | S_REG -> Lwt.return [ file_or_dir, status.st_size ]
    | S_DIR ->
      if SSet.mem ignore_dirs (Filename.basename file_or_dir)
      then Lwt.return []
      else (
        let all = Sys_unix.ls_dir file_or_dir in
        let* files =
          Lwt_list.fold_left_s
            (fun acc f ->
              let* fs = get_all_files (Filename.concat file_or_dir f) in
              Lwt.return (acc @ fs))
            []
            all
        in
        Lwt.return files)
    | S_LNK ->
      (* npm ignores symlinks in the tarball *)
      Lwt.return []
    | S_CHR | S_BLK | S_FIFO | S_SOCK ->
      (* Ignore these types of files as they don't makes sense to include in 
       tarball *)
      Lwt.return []
  in
  Lwt.return files


let from_dir ~dir f =
  let pwd = Core_unix.getcwd () in
  let () = Sys_unix.chdir dir in
  let result = f () in
  let () = Sys_unix.chdir pwd in
  result


let tar ~name ~version dir =
  let open Lwt.Syntax in
  let* files = from_dir ~dir (fun () -> get_all_files ".") in
  let files, sizes = List.unzip files in
  let size = List.fold sizes ~init:0 ~f:( + ) in
  let () = Format.printf "Size = %d\n" size in
  let fcount = List.length files in
  let fname = Filename_unix.temp_file name version in
  let fd =
    Caml_unix.openfile fname [ Core_unix.O_CREAT; Core_unix.O_RDWR ] 0o666
  in
  let () = Tar_unix.Archive.create files fd in
  let () = Caml_unix.close fd in
  Lwt.return (fcount, fname, size)


let tar_gzip ~name ~version dir =
  let open Lwt.Syntax in
  let* fcount, fname, size = tar ~name ~version dir in
  let buf = gzip fname in
  Lwt.return (fcount, Buffer.contents_bytes buf)


let validate_storage ~manifest =
  let open LigoManifest in
  let { main; storage_fn; storage_arg } = manifest in
  match main, storage_fn, storage_arg with
  | Some main, Some storage_fn, Some storage_arg ->
    let () = Printf.printf "Validating storage... %!" in
    let expression = Format.sprintf "%s %s" storage_fn storage_arg in
    let ligo = Sys_unix.executable_name in
    let cmd =
      Cli_helpers.Constants.ligo_compile_storage ~ligo ~main ~expression ()
    in
    let status =
      Lwt_process.with_process_none
        ~stdout:`Dev_null
        ~stderr:`Dev_null
        cmd
        (fun p ->
          Lwt.map
            (fun status ->
              match status with
              | Caml_unix.WEXITED 0 -> Ok ()
              | _ -> Error "unknown error")
            p#status)
    in
    let open Lwt.Syntax in
    let* result = status in
    (match result with
    | Ok _ -> Lwt.return @@ Ok (Printf.printf "Done\n%!")
    | Error _ ->
      Lwt.return
      @@ Error
           "\n\
            Error: Check `storage_fn` & `storage_arg` in packge.json or check \
            your LIGO storage expression")
  | _ -> Lwt.return @@ Ok ()


let validate_main_file ~manifest =
  let open LigoManifest in
  let { main } = manifest in
  Lwt.return
  @@
  match main with
  | Some main ->
    (match Sys_unix.file_exists main with
    | `Yes ->
      (match snd @@ Filename.split_extension main with
      | Some _ -> Ok ()
      | None ->
        Error
          "Invalid LIGO file specifed in main field of package.json\n\
           Valid extension for LIGO files are (.ligo, .mligo, .religo, \
           .jsligo) ")
    | `No | `Unknown ->
      Error
        "main file does not exists.\n\
         Please specify a valid LIGO file in package.json.")
  | None -> Error "No main field in package.json"


let pack ~project_root ~token ~ligo_registry ~manifest =
  let open Lwt.Syntax in
  let LigoManifest.{ name; version; _ } = manifest in
  let () = Format.printf "Packing tarball... %!" in
  let* fcount, tarball = tar_gzip project_root ~name ~version in
  let () = Printf.printf "Done\n%!" in
  let len = Bytes.length tarball in
  let sha1 =
    tarball |> Digestif.SHA1.digest_bytes ~off:0 ~len |> Digestif.SHA1.to_hex
  in
  let sha512 =
    tarball
    |> Digestif.SHA512.digest_bytes ~off:0 ~len
    |> Digestif.SHA512.to_raw_string
  in
  let files_info = FilesInfo.make ~size:len ~sha1 ~sha512 ~fcount ~tarball in
  let* result_main = validate_main_file ~manifest in
  let* result_storage = validate_storage ~manifest in
  match Result.all_unit [ result_main; result_storage ] with
  | Ok () ->
    let body = Body.make ~ligo_registry ~files_info ~manifest in
    Lwt.return @@ Ok body
  | Error e -> Lwt.return @@ Error e


let validate = ()
let publish = ()

let handle_server_response ~name response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
  | `Conflict -> Error ("\nConflict: version already exists", "")
  | `Created ->
    let () = Printf.printf "Done\n%!" in
    Ok ("Package successfully published", "")
  | `Unauthorized ->
    Error
      ( Format.sprintf
          "\n%s already exists and you don't seem to have access to it."
          name
      , "" )
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    Error ("\nRegistry seems down. Contact the developers", "")
  | _ -> Error (body, "")


let ( let* ) x f = Result.bind x ~f

let validate_manifest manifest =
  match Result.bind manifest ~f:LigoManifest.validate with
  | Ok manifest -> Ok manifest
  | Error e -> Error (Format.sprintf "\nERROR: %s" e, "")


let get_auth_token ~ligorc_path ligo_registry =
  let ligorc = LigoRC.read ~ligorc_path in
  let registry_key = LigoRC.registry_key ligo_registry in
  let token = LigoRC.get_token ~registry_key ligorc in
  match token with
  | Some token -> Ok token
  | None ->
    Error ("\nUser not logged in.\nHint: Use ligo login or ligo add-user.", "")


let get_project_root project_root =
  match project_root with
  | Some project_root -> Ok project_root
  | None ->
    Error
      ( "\n\
         Can't find project-root.\n\
         Hint: Use --project-root to specify project root"
      , "" )


let publish ~ligo_registry ~ligorc_path ~project_root ~dry_run =
  let manifest = LigoManifest.read ~project_root in
  let* manifest = validate_manifest manifest in
  let* token = get_auth_token ~ligorc_path ligo_registry in
  let* project_root = get_project_root project_root in
  let body = pack ~project_root ~token ~ligo_registry ~manifest in
  let body =
    (* TODO: fix this... *)
    match Lwt_main.run body with
    | Ok body -> body
    | Error e -> failwith e
  in
  let request =
    if dry_run
    then Lwt.return (Error "TODO: dry-run")
    else http ~token ~body ~ligo_registry ~manifest
  in
  let () = Printf.printf "Uploading package... %!" in
  match Lwt_main.run request with
  | Ok (response, body) ->
    handle_server_response ~name:manifest.name response body
  | Error e -> Error (e, "")
