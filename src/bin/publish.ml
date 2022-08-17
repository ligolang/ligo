(* 

TODO Checklist:

CLI:
- [ ] User facing docs
- [ ] Devs facing docs
- [ ] Code clean up
- [X] Impl ligo publish
  - [X] Use a semver library fro validating verison
  - [X] Read package.json and prepare request body
  - [X] Read .ligorc and get token
  - [X] If no README (.md or any other extension) then "ERROR: No README data found!"
  - [X] Impl shasum & integrity (sha-512)
  - [X] Response Handling
- [X] Impl ligo add-user
  - [X] Response Handling
- [X] Impl ligo login
  - [X] Response Handling
- [ ] Debatable - write tests
- [ ] Write unit test for json body + headers
- [X] Handle errors (duplicate package or version, not authorised, etc.)
- [X] Sanitize manifest (Take care of Semver format, rest of the metadata)
- [ ] .ligoignore ?? (for vbeta ask for only relative paths to ignore)
- [X] login & add-user: CLI prompt for username & password
- [ ] Better logging during publish (show tarball contents, size, etc. similar to npm publish)

DOCS:
- [ ] Mention that only gloable ligorc (~/.ligorc) file

UI:
- [ ] List packages
- [ ] For a package list all versions
- [ ] Stats about pakage (user, downloads) stuff what npmjs website shows

*)

module LigoRC = Cli_helpers.LigoRC
module LigoManifest = Cli_helpers.LigoManifest
module SMap = Caml.Map.Make(String)

type sem_ver = string [@@deriving to_yojson]

type dist_tag = { latest : sem_ver } [@@deriving to_yojson]

type dist = 
  { integrity : string
  ; shasum    : string
  ; tarball   : string
  } [@@deriving to_yojson]
type author = {
    name : string
} [@@deriving to_yojson]
type version = 
  { name        : string
  ; author      : author
  ; version     : sem_ver
  ; description : string
  ; scripts     : (string * string) list
  ; readme      : string
  ; id          : string [@key "_id"]
  ; dist        : dist
  } [@@deriving to_yojson]
 
type attachment =
  { content_type : string
  ; data         : string
  ; length       : int
  } [@@deriving to_yojson]

module Versions = struct
  type t = version SMap.t
  let to_yojson t =
    let kvs = SMap.fold (fun k v xs ->
      (k,version_to_yojson v)::xs  
    ) t [] in
    `Assoc kvs
end

module Attachments = struct
  type t = attachment SMap.t
  let to_yojson t =
    let kvs = SMap.fold (fun k v xs ->
      (k,attachment_to_yojson v)::xs  
    ) t [] in
    `Assoc kvs
end

type body = 
  { id          : string [@key "_id"]
  ; name        : string 
  ; description : string
  ; dist_tags   : dist_tag [@key "dist-tags"]
  ; versions    : Versions.t  
  ; readme      : string
  ; attachments : Attachments.t [@key "_attachments"]
  } [@@deriving to_yojson]

let body ~name ~author ~readme ~version ~ligo_registry ~description ~sha512 ~sha1 ~gzipped_tarball ~scripts = {
  id = name ;
  name ;
  description ;
  dist_tags = {
    latest = version
  } ;
  versions = SMap.add version {
    name ;
    author = {
      name = author
    } ;
    version = version ;
    description ;
    scripts ;
    readme ;
    id = Format.sprintf "%s@%s" name version ;
    dist = {
      integrity = Format.sprintf "sha512-%s" (Base64.encode_exn sha512) ;
      shasum = sha1 ;
      tarball = Format.sprintf "%s/%s/-/%s-%s.tgz" ligo_registry name name version ;
    } ;
  } SMap.empty ;
  readme ;
  attachments = SMap.add (Format.sprintf "%s-%s.tgz" name version) {
    content_type = "application/octet-stream" ;
    data = (Base64.encode_exn (Bytes.to_string gzipped_tarball)) ;
    length = (Bytes.length gzipped_tarball)
  } SMap.empty
}

let http ~token ~sha1 ~sha512 ~gzipped_tarball ~ligo_registry ~manifest =
  let open Cohttp_lwt_unix in
  let LigoManifest.{ name ; version ; scripts ; description ; readme ; author ;  _ } = manifest in
  let uri = Uri.of_string (Format.sprintf "%s/%s" ligo_registry name) in
  let headers = Cohttp.Header.of_list [
    ("referer", "publish") ;
    ("authorization", Format.sprintf "Bearer %s" token) ;
    ("content-type", "application/json") ;
  ] in
  let body = body 
    ~name 
    ~author
    ~version
    ~scripts
    ~description 
    ~readme
    ~ligo_registry
    ~sha512
    ~sha1
    ~gzipped_tarball
    |> body_to_yojson 
    |> Yojson.Safe.to_string 
    |> Cohttp_lwt.Body.of_string in
  Client.put ~headers ~body uri

let os_type =
  match Sys.os_type with
    "Unix" -> Gz.Unix
  | "Win32" -> Gz.NTFS
  | "Cygwin" -> Gz.NTFS
  | _ -> Gz.Unix

let gzip fname fd =
  let file_size = Int.of_int64_exn (Unix.stat fname).st_size in
  let level = 4 in
  let buffer_len = De.io_buffer_size in
  let time () = Int32.of_float (Unix.gettimeofday ()) in
  let i = De.bigstring_create buffer_len in
  let o = De.bigstring_create buffer_len in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let cfg = Gz.Higher.configuration os_type time in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0 then 0 else
    let bytes = Bytes.create len in
    let len = Caml.Unix.read fd bytes 0 len in
    Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len ;
    p := !p + len ; len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o ; 
  r

(* TODO: More files here ?? *)
module SSet = Set.Make(String)
let ignore_dirs 
  = SSet.of_list [ ".ligo" ; "_esy" ; "node_modules" ; "esy.lock" ; ".git" ]

let rec get_all_files : string -> string list Lwt.t = fun file_or_dir ->
  let open Lwt.Syntax in
  let* status = Lwt_unix.lstat file_or_dir in
  let* files = match status.st_kind with
    S_REG -> Lwt.return [file_or_dir]
  | S_DIR ->
    if SSet.mem ignore_dirs (Filename.basename file_or_dir) 
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

let from_dir ~dir f =
  let pwd = Unix.getcwd () in
  let () = Sys.chdir dir in
  let result = f () in
  let () = Sys.chdir pwd in
  result

let tar_gzip ~name ~version dir = 
  let open Lwt.Syntax in
  let* files = from_dir ~dir (fun () -> get_all_files ".") in
  let fname = Filename.temp_file name version in
  let fd = Caml.Unix.openfile fname [ Unix.O_CREAT ; Unix.O_RDWR ] 0o666 in
  let () = Tar_unix.Archive.create files fd in
  let () = Caml.Unix.close fd in
  let fd = Caml.Unix.openfile fname [ Unix.O_RDWR ] 0o666 in
  let buf = gzip fname fd in
  let () = Caml.Unix.close fd in
  Lwt.return (Buffer.contents_bytes buf)

let publish ~project_root ~token ~ligo_registry ~manifest =
  let open Lwt.Syntax in
  let LigoManifest.{ name ; version ; _ } = manifest in
  let () = Format.printf "Packing tarball... %!" in
  let* gzipped_tarball = tar_gzip project_root ~name ~version in
  let () = Printf.printf "Done\n%!" in
  let len = Bytes.length gzipped_tarball in
  let sha1 = gzipped_tarball
    |> Digestif.SHA1.digest_bytes ~off:0 ~len
    |> Digestif.SHA1.to_hex in
  let sha512 = gzipped_tarball 
    |> Digestif.SHA512.digest_bytes ~off:0 ~len
    |> Digestif.SHA512.to_raw_string in
  let () = Printf.printf "Uploading package to LIGO registry... %!" in
  http ~token ~sha1 ~sha512 ~gzipped_tarball ~ligo_registry ~manifest

let handle_server_response ~name response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
    `Conflict -> Error ("\nConflict: version already exists", "")
  | `Created -> 
    let () = Printf.printf "Done\n%!" in
    Ok ("Package successfully published", "")
  | `Unauthorized -> Error (Format.sprintf "\n%s already exists and you don't seem to have access to it." name, "")
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout -> Error ("\nRegistry seems down. Contact the developers", "")
  | _ -> Error (body, "")

let publish ~ligo_registry ~ligorc_path ~project_root =
  let manifest = LigoManifest.read ~project_root in
  let manifest = Result.bind manifest ~f:LigoManifest.validate in
  match manifest with
    Error e -> Error (Format.sprintf "\nERROR: %s" e, "")
  | Ok manifest -> 
    let ligorc = LigoRC.read ~ligorc_path in
    let registry_key = LigoRC.registry_key ligo_registry in
    let token = LigoRC.get_token ~registry_key ligorc in 
    (match token with
      None -> Error ("\nUser not logged in.\nHint: Use ligo login or ligo add-user", "")
    | Some token ->
      let project_root = Option.value_exn project_root in
      let response, body = Lwt_main.run (publish ~project_root ~token ~ligo_registry ~manifest) in
      handle_server_response ~name:manifest.name response body)
