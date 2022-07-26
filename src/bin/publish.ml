(* 

TODO Checklist:

CLI:
- [ ] User facing docs
- [ ] Devs facing docs
- [ ] Code clean up
- [ ] Impl ligo publish
  - [X] Use a semver library fro validating verison
  - [X] Read package.json and prepare request body
  - [X] Read .ligorc and get token
  - [X] If no README (.md or any other extension) then "ERROR: No README data found!"
  - [X] Impl shasum & integrity (sha-512)
  - [ ] Response Handling
- [X] Impl ligo add-user
  - [ ] Response Handling
- [X] Impl ligo login
  - [ ] Response Handling
- [ ] Debatable - write tests
- [ ] Write unit test for json body + headers
- [ ] Handle errors (duplicate package or version, not authorised, etc.)
- [X] Sanitize manifest (Take care of Semver format, rest of the metadata)
- [ ] Error handline via ~raise (Trace)
- [ ] .ligoignore ?? (for vbeta ask for only relative paths to ignore)

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
type version = 
  { name        : string
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

let body ~name ~readme ~version ~ligo_registry ~description ~sha512 ~sha1 ~gzipped_tarball ~scripts = {
  id = name ;
  name ;
  description ;
  dist_tags = {
    latest = version
  } ;
  versions = SMap.add version {
    name ;
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
  let manifest = LigoManifest.validate manifest in
  let LigoManifest.{ name ; version ; scripts ; description ; readme ; _ } = manifest in
  let uri = Uri.of_string (Format.sprintf "%s/%s" ligo_registry name) in
  let headers = Cohttp.Header.of_list [
    ("referer", "publish") ;
    ("authorization", Format.sprintf "Bearer %s" token) ;
    ("content-type", "application/json") ;
  ] in
  let body = body 
    ~name 
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
  let cfg = Gz.Higher.configuration os_type time in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0 then 0 else
    let bytes = Bytes.create len in
    let rd = Caml.Unix.read fd bytes !p len in
    let len = rd in
    Bigstringaf.blit_from_bytes bytes ~src_off:!p buf ~dst_off:0 ~len ;
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

let tar_gzip ~name ~version dir = 
  let open Lwt.Syntax in
  let* files = get_all_files dir in

  let fname = Filename.temp_file name version in

  let* fd = Lwt_unix.openfile fname [ Unix.O_CREAT ; Unix.O_RDWR ] 0o666 in
  let* () = Tar_lwt_unix.Archive.create files fd in

  let* () = Lwt_unix.close fd in
  let* fd = Lwt_unix.openfile fname [ Unix.O_RDWR ] 0o666 in

  let buf = gzip fname (Lwt_unix.unix_file_descr fd) in

  let* () = Lwt_unix.close fd in

  Lwt.return (Buffer.contents_bytes buf)

let publish ~token ~ligo_registry ~manifest =
  let open Lwt.Syntax in
  let LigoManifest.{ name ; version ; _ } = manifest in
  let* gzipped_tarball = tar_gzip "." ~name ~version in
  let len = Bytes.length gzipped_tarball in
  let sha1 = gzipped_tarball
    |> Digestif.SHA1.digest_bytes ~off:0 ~len
    |> Digestif.SHA1.to_hex in
  let sha512 = gzipped_tarball 
    |> Digestif.SHA512.digest_bytes ~off:0 ~len
    |> Digestif.SHA512.to_raw_string in 
  http ~token ~sha1 ~sha512 ~gzipped_tarball ~ligo_registry ~manifest

(*  *)
let publish ~ligo_registry ~ligorc_path ~project_root =
  let open Cohttp in
  let open Cohttp_lwt in

  let manifest = Cli_helpers.LigoManifest.read ~project_root in
  
  let ligorc = LigoRC.read ~ligorc_path in
  let registry_key = LigoRC.registry_key ligo_registry in
  let token = LigoRC.get_token ~registry_key ligorc in 
  
  match token with
    None -> Error ("User not logged in.\nHint: Use ligo login or ligo add-user", "")
  | Some token ->
    let response, body = Lwt_main.run (publish ~token ~ligo_registry ~manifest) in
    let body = Lwt_main.run (Body.to_string body) in
    (* TODO: better error & success message *)
    let code = response |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (response |> Response.headers |> Header.to_string);
    (* TODO: use _body for better errors *)
    Printf.printf "Body: %s\n" body;
    Ok ("", "")