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


TODO Checklist:
CLI:
- [ ] User facing docs
- [ ] Devs facing docs
- [ ] Code clean up
- [ ] Impl ligo publish
  - [ ] Use a semver library fro validating verison
  - [ ] Read package.json and prepare request body
  - [ ] Read .ligorc and get token
  - [ ] If no README (.md or any other extension) then "ERROR: No README data found!"
  - [ ] Impl shasum & integrity (sha-512)
- [ ] Impl ligo add-user
- [ ] Impl ligo login
- [ ] Debatable - write tests
- [ ] Write unit test for json body + headers
- [ ] Handle errors (duplicate package or version, not authorised, etc.)
- [ ] Sanitize manifest (Take care of Semver format, rest of the metadata)

UI:
- [ ] List packages
- [ ] For a package list all versions
- [ ] Stats about pakage (user, downloads) stuff what npmjs website shows

*)

module SMap = Caml.Map.Make(String)

module Scripts = struct
  type t = string SMap.t
  let to_yojson t =
    let kvs = SMap.fold (fun k v xs ->
      (k,`String v)::xs  
    ) t [] in
    `Assoc kvs
end

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
  ; scripts     : Scripts.t
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

let body ~name ~version = {
  id = name ;
  name = name ;
  description = "List helpers for LIGO";
  dist_tags = {
    latest = version
  } ;
  versions = SMap.add version {
    name = "melwyn95_dummy" ;
    version = version ;
    description = "List helpers for LIGO" ;
    scripts = SMap.add "test" "ligo run test list.test.mligo" SMap.empty ;
    readme = "ERROR: No README data found!" ;
    id = Format.sprintf "melwyn95_dummy@%s" version ;
    dist = {
      integrity = "sha512-pwu343Jq5viONVDuZWs6yTm7+uW/17Dw3AJyJWxfQreGUDF+3Gu4o8y8C3IjUrOw1+qfrbhh/jUKfKzSgK2M/w==" ;
      shasum = "81073731fcf981b425cde9b62f647545c8d3a033" ;
      tarball = Format.sprintf "http://registry.npmjs.org/melwyn95_dummy/-/melwyn95_dummy-%s.tgz" version ;
    } ;
  } SMap.empty ;
  readme = "ERROR: No README data found!";
  attachments = SMap.add (Format.sprintf "melwyn95_dummy-%s.tgz" version) {
    content_type = "application/octet-stream" ;
    data = "H4sIAAAAAAAAA+2Vy46iQBSGZ+1TMKyc0JGL3JykF6K22GJUvEC7mYgWCAKFVaDCpN99itaeRaeTWczYnUn4Noec81NVcOqvStab/doD7I7ERoC/3AKO42RRpN7LlygSqTUlmeN5WebIM8dzUlMg8SareUOG0zUiS/nbcS7fQv2O/wkbGOOUAvEGbsGWuqdoXcSD9iuMsCw42B2Z3f4EtOZBd5/MerlcrBy5HUIIJlOYC7KMMmeyj6ap6aJsjg7GLn8UOWOxUFE6isassdKHT0knQ6kyNFNOl6cPbOtsTk9niCTdUAxd3Zk5Z2vjjjrcRcd+tAxGo54yK8x+qyvb3WUsOSvFG7qjRQEdo2/lVoIxtwQrbWjZfj5PLVvqQmVrMY8yiPuMqQYTT7fYyWI1UQvWeFA3RaHa3sGW2JPKjDzNn/nNR91sp7y9isNsKCnIBvtVBo+LY2wJmV90oiDPH5wZz0iplwU4klqeBpJxMO+eg+QJMaKrMa6nMv7QFHo+G5jnQIBORxabPW3s4fag5fYGcsebtRghd+OD1hsk/Sd/z5qzJV+02yfyg+/v6VqtdmmC48ekAVrmugA1XASj+rUtdxTtrDGQRfrbq9bFRIrAIfMRqNMuLisubpyQn4IHPwSzPN7U6RdTp15B35WDf3uv/8nV/ynA6Sf5n2u+43+l8v+HUG4nGIJGCL06rYMwhJQFUbj9SnbUZ6+t4va8+v8ayREA4389xx/8L4jcW//zklj5/0P4WaMoOl5HgP5O0REIT3nckn5ssyjK6buydgQI+zAuy3yDa/DNS3oL8Ab5SXotGT65lHYgTIiaciGijEF/fFFedJioyrlIorxqyndC34MUymKqTFAhGaHxcgtFZYEm2ufac3UGVVRUVNyIX+/A8pgAEAAA" ;
    length = 681
  } SMap.empty
}

let http ~token =
  let open Cohttp_lwt_unix in
  let pkg_name = "melwyn95_dummy" in
  let uri = Uri.of_string (Format.sprintf "%s/%s" Cli_helpers.Constants.ligo_registry pkg_name) in
  let headers = Cohttp.Header.of_list [
    ("referer", "publish") ;
    ("authorization", Format.sprintf "Bearer %s" token) ;
    ("content-type", "application/json") ;
  ] in
  let body = body ~name:pkg_name ~version:"1.0.16"
    |> body_to_yojson 
    |> Yojson.Safe.to_string 
    |> Cohttp_lwt.Body.of_string in
  Client.put ~headers ~body uri

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
  (* let cwd = Unix.getcwd () in
  let enc = Lwt_main.run (tar_gzip_base64 cwd) in
  print_endline enc; *)
  let request = http ~token:"ZTM1N2QxNDBiM2E0YzY4OGVmZTA0ZGNkNDRmOWIyYzU6ZmZlZjE2ODQ3NmE1YzA=" in
  let open Cohttp in
  let open Cohttp_lwt in
  let response,body = Lwt_main.run request in
  let code = response |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (response |> Response.headers |> Header.to_string);
  (* body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body); *)
  Ok ("", "")