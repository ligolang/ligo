(* 


- [x] Add --dry-run CLI flag to ligo publish
- [x] when directory field is null don't send the key
- [x] Stat main file before publish
- [x] Implement --dry-run flag
- [x] Add stats about packed size, upacked size, total files in json
- [x] Show tarball contents (number of files) & tarball details in CLI output (name, version, filenam[tarball], packed size, unpacked size, shasum, integrity, total files)
- [x] Wrap logging message in a function ~before ~after
- [x] Add support for .ligoignore to igore stuff while packaging
- [x] Refactor manifest parsing & validation
- [x] Add unit tests for manifest parsing & validation
- [ ] Add expect tests for ligo publish --dry-run which check for valid storage_fn, storage_arg, main
- [ ] 2 tests for tar-gzip (< 1 MB & > 1 MB)
- [ ] Add basic comments in code
- [ ] Docs: Update docs related to recent changes to package.json (Docs: manifest file)
- [ ] Docs: Add note about #import/include"<pkg>/<path>"
- [ ] Docs: Add note about `--registry`
- [ ] manually Test CLI option to override path to .ligorc
- [ ] manually Test .ligoignore stuff
- [ ] manually end-to-end test publishing & installing pacakges
*)

module LigoRC = Cli_helpers.LigoRC
module LigoManifest = Cli_helpers.LigoManifest
module LigoIgnore = Cli_helpers.LigoIgnore
module RepositoryUrl = Cli_helpers.RepositoryUrl
module Semver = Cli_helpers.Semver
module SMap = Caml.Map.Make (String)

type object_ = (string * string) list

let object__to_yojson o =
  `Assoc (List.fold o ~init:[] ~f:(fun kvs (k, v) -> (k, `String v) :: kvs))


let with_logging ~before ?(after = "Done") fn =
  let () = Printf.printf "==> %s... %!" before in
  match fn () with
  | Ok v ->
    let () = Printf.printf "%s\n%!" after in
    Ok v
  | Error e -> Error e


type sem_ver = string [@@deriving to_yojson]
type dist_tag = { latest : sem_ver } [@@deriving to_yojson]

module PackageStats = struct
  type t =
    { name : string
    ; version : Semver.t
    ; file_count : int
    ; unpacked_size : int
    ; packed_size : int
    ; tarball_name : string
    ; tarball_content : bytes
    ; sha1 : string
    ; sha512 : string
    ; integrity : string
    }

  let make
      ~name
      ~version
      ~unpacked_size
      ~packed_size
      ~fcount
      ~sha1
      ~sha512
      ~tarball
    =
    { name
    ; version
    ; file_count = fcount
    ; unpacked_size
    ; packed_size
    ; tarball_name = Format.sprintf "%s-%s.tgz" name (Semver.to_string version)
    ; tarball_content = tarball
    ; sha1
    ; sha512
    ; integrity = Format.sprintf "sha512-%s" (Base64.encode_exn sha512)
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

  let make ~tarball ~package_stats =
    let PackageStats.{ sha1; file_count; unpacked_size; integrity; _ } =
      package_stats
    in
    { integrity; shasum = sha1; tarball; file_count; unpacked_size }
end

type author = { name : string } [@@deriving to_yojson]

module Version = struct
  type t =
    { name : string
    ; author : author
    ; main : string
    ; type_ : string [@key "type"]
    ; storage_fn : string option
    ; storage_arg : string option
    ; repository : RepositoryUrl.t
    ; version : Semver.t
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

  let make ~ligo_registry ~package_stats ~manifest =
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
      Format.sprintf
        "%s/%s/-/%s"
        ligo_registry
        name
        package_stats.PackageStats.tarball_name
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
    ; id = Format.sprintf "%s@%s" name (Semver.to_string version)
    ; dist = Dist.make ~tarball ~package_stats
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

  let make ~ligo_registry ~package_stats ~manifest =
    let LigoManifest.{ name; version; readme; description; _ } = manifest in
    let gzipped_tarball = package_stats.PackageStats.tarball_content in
    let v = Version.make ~ligo_registry ~package_stats ~manifest in
    let version = Semver.to_string version in
    let versions = SMap.add version v SMap.empty in
    { id = name
    ; name
    ; description
    ; dist_tags = { latest = version }
    ; versions
    ; readme
    ; attachments =
        SMap.add
          package_stats.PackageStats.tarball_name
          Attachment.
            { content_type = "application/octet-stream"
            ; data = Base64.encode_exn (Bytes.to_string gzipped_tarball)
            ; length = Bytes.length gzipped_tarball
            }
          SMap.empty
    }
end

let handle_server_response ~name response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
  | `Conflict -> Error ("\nConflict: version already exists", "")
  | `Created -> Ok ("Package successfully published", "")
  | `Unauthorized ->
    Error
      ( Format.sprintf
          "\n%s already exists and you don't seem to have access to it."
          name
      , "" )
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    Error ("\nRegistry seems down. Contact the developers", "")
  | _ -> Error (body, "")


let publish ~ligo_registry ~manifest ~body ~token =
  let open Cohttp_lwt_unix in
  let LigoManifest.{ name; _ } = manifest in
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
  let response, body = Lwt_main.run r in
  handle_server_response ~name:manifest.name response body


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


let rec get_all_files
    : ligoignore:(string -> bool) -> string -> (string * int) list Lwt.t
  =
 fun ~ligoignore file_or_dir ->
  let open Lwt.Syntax in
  let* status = Lwt_unix.lstat file_or_dir in
  let* files =
    match status.st_kind with
    | S_REG ->
      if ligoignore (String.chop_prefix_if_exists ~prefix:"." file_or_dir)
      then Lwt.return []
      else Lwt.return [ file_or_dir, status.st_size ]
    | S_DIR ->
      if ligoignore (String.chop_prefix_if_exists ~prefix:"." file_or_dir)
      then Lwt.return []
      else (
        let all = Sys_unix.ls_dir file_or_dir in
        let* files =
          Lwt_list.fold_left_s
            (fun acc f ->
              let* fs =
                get_all_files ~ligoignore (Filename.concat file_or_dir f)
              in
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


let tar ~name ~version files =
  let files, sizes = List.unzip files in
  let unpacked_size = List.fold sizes ~init:0 ~f:( + ) in
  let fcount = List.length files in
  let fname = Filename_unix.temp_file name (Semver.to_string version) in
  let fd =
    Caml_unix.openfile fname [ Core_unix.O_CREAT; Core_unix.O_RDWR ] 0o666
  in
  let () = Tar_unix.Archive.create files fd in
  let () = Caml_unix.close fd in
  Lwt.return (fcount, fname, unpacked_size)


let tar_gzip ~name ~version ~ligoignore dir =
  let open Lwt.Syntax in
  let* files = from_dir ~dir (fun () -> get_all_files ~ligoignore ".") in
  let* fcount, fname, unpacked_size = tar ~name ~version files in
  let buf = gzip fname in
  Lwt.return (fcount, Buffer.contents_bytes buf, unpacked_size)


let pack ~project_root ~ligo_registry ~ligoignore ~manifest =
  let LigoManifest.{ name; version; _ } = manifest in
  let fcount, tarball, unpacked_size =
    Lwt_main.run @@ tar_gzip project_root ~name ~version ~ligoignore
  in
  let packed_size = Bytes.length tarball in
  let sha1 =
    tarball
    |> Digestif.SHA1.digest_bytes ~off:0 ~len:packed_size
    |> Digestif.SHA1.to_hex
  in
  let sha512 =
    tarball
    |> Digestif.SHA512.digest_bytes ~off:0 ~len:packed_size
    |> Digestif.SHA512.to_hex
  in
  let package_stats =
    PackageStats.make
      ~name
      ~version
      ~unpacked_size
      ~packed_size
      ~sha1
      ~sha512
      ~fcount
      ~tarball
  in
  let body = Body.make ~ligo_registry ~package_stats ~manifest in
  Ok (body, package_stats)


let ( let* ) x f = Result.bind x ~f

let read_manifest ~project_root =
  let manifest = LigoManifest.read ~project_root in
  Result.map_error manifest ~f:(fun e -> e, "")


let validate_manifest manifest =
  match LigoManifest.validate manifest with
  | Ok () -> Ok ()
  | Error e -> Error (Format.sprintf "\nError: %s" e, "")


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
         Hint: Use --project-root to specify project root."
      , "" )


let show_stats stats =
  let PackageStats.
        { name
        ; version
        ; tarball_name
        ; packed_size
        ; unpacked_size
        ; sha1
        ; sha512
        ; file_count
        ; _
        }
    =
    stats
  in
  let human_readable_size size =
    let kilo = 1024.0 in
    let mega = kilo *. kilo in
    let giga = kilo *. mega in
    match float_of_int size with
    | size when Float.(size >= giga) -> Format.sprintf "%0.2f GB" (size /. giga)
    | size when Float.(size >= mega) -> Format.sprintf "%0.2f MB" (size /. mega)
    | size when Float.(size >= kilo) -> Format.sprintf "%0.2f kB" (size /. kilo)
    | size -> Format.sprintf "%d B" (int_of_float size)
  in
  let prefix = String.sub sha512 ~pos:0 ~len:13 in
  let suffix = String.sub sha512 ~pos:(String.length sha512 - 15) ~len:15 in
  let version = Semver.to_string version in
  let integrity = Format.sprintf "sha512-%s[...]%s" prefix suffix in
  let () = Format.printf "    publishing: %s@%s\n%!" name version in
  let () = Format.printf "    === Tarball Details ===\n%!" in
  let () = Format.printf "    name:          %s\n%!" name in
  let () = Format.printf "    version:       %s\n%!" version in
  let () = Format.printf "    filename:      %s\n%!" tarball_name in
  let () =
    Format.printf "    package size:  %s\n%!" (human_readable_size packed_size)
  in
  let () =
    Format.printf
      "    unpacked size: %s\n%!"
      (human_readable_size unpacked_size)
  in
  let () = Format.printf "    shasum:        %s\n%!" sha1 in
  let () = Format.printf "    integrity:     %s\n%!" integrity in
  let () = Format.printf "    total files:   %d\n%!" file_count in
  ()


let publish ~ligo_registry ~ligorc_path ~ligoignore_path ~project_root ~dry_run =
  let* manifest =
    with_logging ~before:"Reading manifest" (fun () ->
        read_manifest ~project_root)
  in
  let* () =
    with_logging ~before:"Validating manifest file" (fun () ->
        validate_manifest manifest)
  in
  let* project_root =
    with_logging ~before:"Finding project root" (fun () ->
        get_project_root project_root)
  in
  let ligoignore = LigoIgnore.matches @@ LigoIgnore.read ~ligoignore_path in
  let* packed, stats =
    with_logging ~before:"Packing tarball" (fun () ->
        pack ~project_root ~ligo_registry ~ligoignore ~manifest)
  in
  let () = show_stats stats in
  if dry_run
  then Ok ("", "")
  else
    let* token =
      with_logging ~before:"Checking auth token" (fun () ->
          get_auth_token ~ligorc_path ligo_registry)
    in
    with_logging ~before:"Uploading package" (fun () ->
        publish ~token ~body:packed ~ligo_registry ~manifest)
