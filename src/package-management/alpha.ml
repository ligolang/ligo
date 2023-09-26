module Json = Yojson.Safe
module Cohttp_lwt = Cohttp_lwt
module Cohttp_lwt_unix = Cohttp_lwt_unix
open Package_management_alpha_files
open Lock_file
open Cli_helpers

module Closure = Caml.Set.Make (struct
  include NameVersion

  type t = NameVersion.t
end)

module Metadata_cache = Caml.Map.Make (struct
  include NameVersion

  type t = NameVersion.t
end)

module InstallationJsonMap = Caml.Map.Make (NameVersion)

let ( let@ ) o f = Result.bind ~f o

type error =
  | NotSupportedInAlpha
  | UnableToAccessRegistry
  | UnableToSerializeSemver
  | UnableToAccessId
  | UnableToAccessName of string
  | SemverParserFailure of string
  | MetadataJsonCacheFail
  | WriteFileFailed of string * string
  | DirectoryCreationFailed of string
  | FetchingTarballFailed of int
  | UntarFailed of string
  | IntegrityMismatch of string
  | InstallationJsonGenerationFailed
  | UnableToObtainRelativePath
  | ProjectRootEmpty
  | LockFileAccessFailed of string
  | InstallTestFails of string
  | Manifest_not_found
  | Lock_file_not_found
  | LockFileParserFailure of string
  | PackageJsonEmptyName
  | RootGenerationInLockFileFailed of string
  | NodeGenerationInLockFileFailed of string
  | LockFileGenerationFailed
  | InternalError of string

let string_of_error = function
  | NotSupportedInAlpha ->
    "Not supported under --package-management-alpha please try with"
  | UnableToAccessRegistry ->
    "Unable to access registry, cannot download the metadata json"
  | UnableToSerializeSemver -> "The version provided is illegal Semver"
  | UnableToAccessId -> "The access id doesn't exists"
  | UnableToAccessName msg -> "The access name doesn't exists\n" ^ msg
  | SemverParserFailure s -> "Could not parse version: " ^ s
  | MetadataJsonCacheFail -> "Cache doesn't contain the metadata"
  | WriteFileFailed (path, msg) ->
    Printf.sprintf "Cannot write to path : %s\n Error Message : %s" path msg
  | DirectoryCreationFailed msg -> msg
  | FetchingTarballFailed status_code ->
    Printf.sprintf
      "Fetching Tarball failed\n Error : %d\n Please restart installation"
      status_code
  | UntarFailed msg -> msg
  | IntegrityMismatch msg -> msg
  | InstallationJsonGenerationFailed -> "Generation of a key in installation.json failed"
  | UnableToObtainRelativePath -> "Unable to obtain a relative path"
  | ProjectRootEmpty -> "Project Root is unavailable"
  | LockFileAccessFailed msg -> msg
  | InstallTestFails msg -> msg
  | Lock_file_not_found -> "No lock file found."
  | LockFileParserFailure m ->
    "Failed to parse lockfile: Reason: " ^ m
    (* TODO : add a helpful message sharing where the failure occured *)
  | Manifest_not_found ->
    "No manifest file found." (*TODO link documentation to what is a ligo manifest file *)
  | PackageJsonEmptyName -> "name field in ligo.json is empty/null"
  | RootGenerationInLockFileFailed msg -> msg
  | NodeGenerationInLockFileFailed msg -> msg
  | LockFileGenerationFailed -> "Lock file generation failed"
  | InternalError msg -> "Internal Error: Reason: " ^ msg


type manifest_result =
  [ `Invalid_ligo_json
  | `Invalid_esy_json
  | `Invalid_package_json
  | `Valid_package_json
  | `Valid_esy_json
  | `No_manifest
  | `OK
  ]

let does_json_manifest_exist ~project_root =
  let package_json = Filename.concat project_root "package.json" in
  let ligo_json = Filename.concat project_root "ligo.json" in
  let esy_json = Filename.concat project_root "esy.json" in
  match
    ( Caml.Sys.file_exists ligo_json
    , Caml.Sys.file_exists package_json
    , Caml.Sys.file_exists esy_json )
  with
  | true, _, _ ->
    (try
       let _ = Yojson.Safe.from_file ligo_json in
       `OK
       (* TODO: in case of invalid json write {} to the file *)
     with
    | _ -> `Invalid_ligo_json)
  | false, true, false ->
    (try
       let _ = Yojson.Safe.from_file package_json in
       `Valid_package_json
     with
    | _ -> `Invalid_package_json)
  | false, _, true ->
    (try
       let _ = Yojson.Safe.from_file esy_json in
       `Valid_esy_json
     with
    | _ -> `Invalid_package_json)
  | false, false, false -> `No_manifest


(* 
[DEPRECATED USAGE IN FUTURE]

With esy not being used this particular convention would be removed
this is just a stub to make sure that currently this works with package import
with the current preproccessor that accepts a package name with a 8 letter hex code 

  This is temporary and ffffffff is random string and has no signficant meaning
*)
let generate_random_uuid () = "ffffffff"
let is_json_empty : Json.t -> bool = fun json -> Json.Util.keys json |> List.is_empty
let ( // ) a b = a ^ "/" ^ b

let trim_quotes : string -> string =
 fun s -> String.strip s ~drop:(fun ch -> Char.equal ch '\"')


let lock_file : string -> Fpath.t =
 fun path -> Fpath.(v path / "ligo.esy.lock" / "index.json")


let ligo_json : string -> Fpath.t =
 fun project_root -> Fpath.(v project_root / "ligo.json")


let installation_json : string -> Fpath.t =
 fun path -> Fpath.(v path / "_ligo" / "ligo" / "installation.json")


let index_json_exists : string -> bool =
 fun path -> Caml.Sys.file_exists @@ Fpath.to_string @@ lock_file path


let ligo_json_exists : string -> bool =
 fun project_root -> Caml.Sys.file_exists @@ Fpath.to_string @@ ligo_json project_root


let read_lock_file ~project_root =
  match index_json_exists project_root with
  | false -> Error Lock_file_not_found
  | true -> Result.return @@ Json.from_file @@ Fpath.to_string @@ lock_file project_root


let read_ligo_json ~project_root =
  match ligo_json_exists project_root with
  | true -> Json.from_file @@ Fpath.to_string @@ ligo_json project_root
  | false -> `Assoc [ "dependencies", `Assoc [] ]


let mkdir_p : Fpath.t -> (unit, error) Lwt_result.t =
 fun path ->
  Bos.OS.Dir.create ~path:true path
  |> function
  | Error (`Msg msg) -> Lwt_result.fail (DirectoryCreationFailed msg)
  | Ok _ -> Lwt_result.return ()


(* TODO : replace with a permanent `find_apt_version` function *)
let find_latest_version : Json.t -> string =
 fun json ->
  Json.Util.(member "dist-tags" json |> member "latest" |> Json.to_string |> trim_quotes)


(* 
if version contains the following
  1. '^' -> return the version that it is prefixed to - eg. "^1.0.0" => "1.0.0"
  2. '~' -> return the version that it is prefixed to - eg. "~1.0.0" => "1.0.0"
  3. '<' -> TODO : ranges are not handled, right now it hard fails
  4. '<=' -> TODO : ranges are not handled, right now it hard fails
  5. '>' -> TODO : ranges are not handled, right now it hard fails
  6. '>=' -> TODO : ranges are not handled, right now it hard fails

  Note : Semver is not respected hence we just return the version or the latest version if it is a "*"
*)

let remove_version_prefix : string -> string =
 fun version ->
  let fail_on_ranges s =
    String.strip s
    |> String.split ~on:' '
    |> List.hd
    |> function
    | Some "<" -> failwith "ranges like < are not handled"
    | Some "<=" -> failwith "ranges like <= are not handled"
    | Some ">=" -> failwith "ranges like >= are not handled"
    | Some ">" -> failwith "ranges like > are not handled"
    | Some version -> version
    | None -> failwith "Wrong prefix attached to the version"
  in
  let strip_caret s = String.strip s ~drop:(Char.equal '^') in
  let strip_tilde s = String.strip s ~drop:(Char.equal '~') in
  version |> fail_on_ranges |> strip_caret |> strip_tilde


let get_apt_version : string -> json:Json.t -> string =
 fun version ~json ->
  remove_version_prefix version
  |> function
  | "*" -> find_latest_version json
  | version -> version


(* The one place where package version must be supplied as a string.
   This helps us pass "*" as a version.
   
   If we disallow "*", find_latest_version is pointless.

   Ideally, it should be decomposed - it shouldnt have to resolve "*" versions
   
 *)

let get_metadata_json
    : name:string -> version:string -> ligo_registry:Uri.t -> (Json.t, error) result Lwt.t
  =
 fun ~name ~version ~ligo_registry ->
  let open Lwt.Syntax in
  (* TODO move this inside registry folder containing all registry interaction logic *)
  let endpoint_uri = Printf.sprintf "/-/api/%s" name in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let* res, body = Cohttp_lwt_unix.Client.get uri in
  let status = Cohttp.Code.code_of_status res.status in
  if status |> Cohttp.Code.is_success
  then
    let* body = Cohttp_lwt.Body.to_string body in
    let json = Json.from_string body in
    let version = get_apt_version version ~json in
    try
      let json = Json.Util.(member "versions" json |> member version) in
      Lwt.return_ok json
    with
    | Yojson.Safe.Util.Type_error (msg, _) -> Lwt.return_error (UnableToAccessName msg)
  else Lwt.return_error UnableToAccessRegistry


(* get the .tgz file, extract it and generate a directory from the .tgz file *)
let fetch_tarball : Uri.t -> string -> (Fpath.t, error) Lwt_result.t =
 fun url filename ->
  let open Lwt.Syntax in
  let* res, body = Cohttp_lwt_unix.Client.get url in
  let status_code = Cohttp.Code.code_of_status res.status in
  if Cohttp.Code.is_success status_code
  then (
    let stream = Cohttp_lwt.Body.to_stream body in
    let* _ =
      Lwt_io.with_file ~mode:Lwt_io.output filename (fun chan ->
          Lwt_stream.iter_s (Lwt_io.write chan) stream)
    in
    Lwt_result.return @@ Fpath.v filename)
  else Lwt_result.fail @@ FetchingTarballFailed status_code


(* manually create the directory where each tarball is extracted *)
let create_pkg_dir ~dest_dir ~tarball_path =
  let open Lwt_result.Syntax in
  let* () = mkdir_p (Fpath.v dest_dir) in
  let res = unzip tarball_path |> Result.map ~f:(untar ~dest_dir) in
  match res with
  | Error UnableToUnzip ->
    Lwt_result.fail @@ UntarFailed ("Unable to untar: " ^ tarball_path)
  | Ok () -> Lwt_result.return ()


let cached_get_metadata_json ~cache ~name ~version ~ligo_registry =
  let open Lwt_result.Syntax in
  match Metadata_cache.find_opt (NameVersion.make ~name ~version) cache with
  | Some metadata_json -> Lwt.return_ok (metadata_json, cache)
  | None ->
    let* (metadata_json : Json.t) =
      get_metadata_json
        ~name
        ~version:(NameVersion.version_to_string version)
        ~ligo_registry
    in
    Lwt.return_ok
      ( metadata_json
      , Metadata_cache.(add NameVersion.{ name; version } metadata_json cache) )


(* The following function fetches tarballs using the source entry in index.json,
  extracts the file, generates the package directory and cleans up the .tgz and .tar files *)
let fetch_tarballs
    :  ligo_registry:Uri.t -> project_root:string -> cache:Json.t Metadata_cache.t
    -> solution:NameVersion.t list -> (Fpath.t InstallationJsonMap.t, error) Lwt_result.t
  =
 fun ~ligo_registry ~project_root ~cache ~solution ->
  let open Lwt_result.Syntax in
  let source = project_root // ".ligo" // "source" // "i" in
  let* _ = mkdir_p (Fpath.v source) in
  (* get the tarball url and shasum *)
  let get_tarball_url_and_shasum json =
    let url =
      Json.Util.(member "dist" json |> member "tarball" |> to_string) |> trim_quotes
    in
    let shasum =
      Json.Util.(member "dist" json |> member "shasum" |> to_string) |> trim_quotes
    in
    url, shasum
  in
  (* loop to fetch the tarballs as .tgz file *)
  let rec loop ~cache ~installation_map pkgs =
    match pkgs with
    | [] -> Lwt_result.return installation_map
    | pkg :: rest ->
      let NameVersion.{ name; version } = pkg in
      let* json, cache = cached_get_metadata_json ~cache ~name ~version ~ligo_registry in
      let url, shasum = get_tarball_url_and_shasum json in
      let tarball_filename =
        let drop ch = Char.equal '@' ch in
        let f ch = if Char.equal '/' ch then "-" else Char.to_string ch in
        Json.Util.(member "name" json |> to_string)
        |> trim_quotes
        |> String.strip ~drop
        |> String.concat_map ~f
      in
      let* version =
        match version with
        | NameVersion.StringVersion version ->
          Lwt_result.fail
            (InternalError
               (Printf.sprintf
                  "While fetching package: default node should not be here\n\
                  \ This version : %s is not Semver compliant"
                  version))
        | NameVersion.SemverVersion version ->
          Lwt_result.return @@ Semver.to_string version
      in
      let tarball_filename = tarball_filename ^ "-" ^ version ^ ".tgz" in
      let tarball_path = source // tarball_filename in
      let* _filename = fetch_tarball (Uri.of_string url) tarball_path in
      let* () =
        Cli_helpers.Checksum.check_integrity tarball_path ~expected:shasum
        |> Result.map_error ~f:(fun e ->
               IntegrityMismatch (Cli_helpers.Checksum.string_of_error e))
        |> Lwt_result.lift
      in
      let dirname =
        let f ch =
          match ch with
          | '/' -> "__s__"
          | '-' -> "_"
          | _ -> Char.to_string ch
        in
        let drop ch = Char.equal ch '@' in
        String.strip ~drop name |> String.concat_map ~f
      in
      let dest_dir =
        source // (dirname ^ "__" ^ version ^ "__" ^ generate_random_uuid ())
      in
      let* rel_dir =
        let path = Fpath.v dest_dir |> Fpath.relativize ~root:(Fpath.v project_root) in
        Lwt_result.lift @@ Result.of_option ~error:UnableToObtainRelativePath path
      in
      let* () = create_pkg_dir ~dest_dir ~tarball_path in
      let installation_map = InstallationJsonMap.add pkg rel_dir installation_map in
      loop ~cache ~installation_map rest
  in
  (* gets details from cache and download tar file to the ligo_package_dir *)
  let* installation_map =
    solution
    |> List.filter ~f:(fun a -> not (NameVersion.is_root a))
    |> loop ~installation_map:InstallationJsonMap.empty ~cache
  in
  (* extract the tarball to a directory in ligo_package_dir *)
  (* collect the leftover .tgz and .tar files as zip_files *)
  let tar_zip_files =
    let dir = Fpath.v source in
    let is_tar_zip path =
      let path = Caml.Filename.extension @@ Fpath.to_string path in
      match path with
      | ".tgz" | ".tar" | ".gzip" -> true
      | _ -> false
    in
    Bos.OS.Dir.contents dir
    |> function
    | Ok content_paths ->
      List.filter content_paths ~f:is_tar_zip |> List.map ~f:Fpath.to_string
    | Error (`Msg msg) -> raise @@ Failure msg
  in
  (* cleanup all the uneccessary .tgz and .tar files in the ligo package directory *)
  List.iter ~f:Caml.Sys.remove tar_zip_files;
  Lwt_result.return installation_map


let generate_default_installation ligo_json project_root =
  let name =
    if is_json_empty ligo_json
    then Fpath.v project_root |> Fpath.segs |> List.last_exn
    else Json.(Util.member "name" ligo_json |> to_string |> trim_quotes)
  in
  let key : string = name ^ "@" ^ "link-dev:./ligo.json" in
  let value = project_root in
  key, value


let generate_default_installation_json : Json.t -> string -> string * Json.t =
 fun ligo_json project_root ->
  let key, value = generate_default_installation ligo_json project_root in
  key, `String value


let generate_installation_json
    : Fpath.t InstallationJsonMap.t -> string -> ligo_json:Json.t -> Json.t
  =
 fun installation_json_map project_root ~ligo_json ->
  let create_json lst : Json.t =
    let f (name_version, path) =
      let key = NameVersion.to_string name_version in
      let value = `String ("./" ^ Fpath.to_string path) in
      key, value
    in
    let default = generate_default_installation_json ligo_json project_root in
    `Assoc (default :: List.map ~f lst)
  in
  let lst = InstallationJsonMap.bindings installation_json_map in
  create_json lst


let assoc_to_dep_list : (string * Json.t) list -> (string * string) list =
 fun assoc_list ->
  List.map assoc_list ~f:(fun (name, version) ->
      name, version |> Json.to_string |> trim_quotes)


(* Get a list of tuples which are devDependencies from ligo.json of type : (package name, package version) list *)
let get_dev_dep_list : Json.t -> (string * string) list =
 fun ligo_json ->
  let dependencies = Json.Util.member "devDependencies" ligo_json in
  Json.Util.(to_option to_assoc dependencies)
  |> function
  | None -> []
  | Some lst -> assoc_to_dep_list lst


let remove_dir : Fpath.t -> unit =
 fun path_to_dir ->
  Bos.OS.Dir.delete path_to_dir ~recurse:true
  |> function
  | Error _ ->
    failwith @@ Format.asprintf "Unable to remove directory : %a" Fpath.pp path_to_dir
  | Ok _ -> ()


(* REBUILD HERE *)
let generate_source json =
  let type_ = Json.Util.(member "type" json |> to_string) |> trim_quotes in
  let dist =
    let shasum =
      Json.Util.(member "dist" json |> member "shasum" |> to_string) |> trim_quotes
    in
    let tarball =
      Json.Util.(member "dist" json |> member "tarball" |> to_string) |> trim_quotes
    in
    "archive:" ^ tarball ^ "#sha1:" ^ shasum
  in
  Source.{ type_; source = [ dist ] }


let field_to_tuple_list ~field json =
  let get_name_and_version (name, version) =
    let name = trim_quotes name in
    let version = Json.Util.(to_string version) |> trim_quotes in
    name, version
  in
  Json.Util.(member field json |> to_option to_assoc)
  |> function
  | Some lst -> List.map lst ~f:get_name_and_version |> Result.return
  | None -> Ok []


let nameVersion_of_tuple (name, version) =
  let version = remove_version_prefix version in
  match Semver.of_string version with
  | Some version ->
    let version = NameVersion.SemverVersion version in
    Ok (NameVersion.make ~name ~version)
  | None -> Error (SemverParserFailure version)


let nameVersion_of_json json =
  let name = Json.Util.(member "name" json |> to_string) |> trim_quotes in
  let version = Json.Util.(member "version" json |> to_string) |> trim_quotes in
  nameVersion_of_tuple (name, version)


let generate_node json (* registry response *) =
  let id =
    (Json.Util.(member "_id" json |> to_string) |> trim_quotes)
    ^ "@"
    ^ generate_random_uuid ()
  in
  let@ NameVersion.{ name; version } = nameVersion_of_json json in
  let source = Source.Node (generate_source json) in
  let dependencies_tuple_to_name_version dependencies =
    let f acc (name, version) =
      match acc with
      | Ok xs ->
        let version = remove_version_prefix version in
        (match Semver.of_string version with
        | Some version ->
          let version = NameVersion.SemverVersion version in
          Ok (NameVersion.make ~name ~version :: xs)
        | None -> Error (SemverParserFailure version))
      | e -> e
    in
    List.fold_left ~f ~init:(Ok []) dependencies
  in
  let@ dependencies = field_to_tuple_list ~field:"dependencies" json in
  let@ dependencies = dependencies_tuple_to_name_version dependencies in
  let@ dev_dependencies = field_to_tuple_list ~field:"devDependencies" json in
  let@ dev_dependencies = dependencies_tuple_to_name_version dev_dependencies in
  let overrides = [] in
  let version =
    match version with
    | NameVersion.SemverVersion version -> Node.NodeVersion version
    | NameVersion.StringVersion version -> Node.DefaultVersion version
  in
  Ok Node.{ id; name; version; source; overrides; dependencies; dev_dependencies }


let generate_default_node
    :  project_name:string -> ligo_json:Json.t -> dependencies:NameVersion.t list
    -> dev_dependencies:NameVersion.t list -> (Node.t, error) result
  =
 fun ~project_name ~ligo_json ~dependencies ~dev_dependencies ->
  let open Json in
  let name =
    match Util.(member "name" ligo_json) with
    | `String name -> name
    | _ -> project_name
  in
  let name = name |> trim_quotes in
  let id = name ^ "@" ^ "link-dev:./ligo.json" in
  let version = Node.DefaultVersion "link-dev:./ligo.json" in
  let source =
    let type_ = "link-dev" in
    let path = "." in
    let manifest = "ligo.json" in
    Source.Default Source.{ type_; path; manifest }
  in
  let overrides = [] in
  Ok Node.{ id; name; version; source; overrides; dependencies; dev_dependencies }


(* let node : (string * t) list = [ "node", node_value ] in *)
(* `Assoc (root :: node) *)

module RegistryResponse : sig
  val get_version : (Json.t, error) result -> (string, error) result
  val get_dependencies : (Json.t, error) result -> (string * string) list
end = struct
  let get_version metadata_json =
    match metadata_json with
    | Ok json ->
      Json.Util.(member "version" json |> to_string) |> trim_quotes |> Result.return
    | Error e -> Error e


  let get_dependencies metadata_json =
    match metadata_json with
    | Ok json ->
      Json.Util.(member "dependencies" json |> to_option to_assoc)
      |> (function
      | None -> []
      | Some lst ->
        List.map lst ~f:(fun (name, version) ->
            let version = Json.Util.to_string version |> trim_quotes in
            name, version))
    | Error _ -> []
end

let get_transitive_closure ~ligo_registry dependencies =
  let open Lwt_result.Syntax in
  let rec traverse deps closure cache =
    match deps with
    | [] -> Lwt.return_ok (Closure.elements closure, cache)
    | (name, version) :: rest ->
      let version_str = remove_version_prefix version in
      let* version =
        match Semver.of_string version_str with
        | Some version -> Lwt.return_ok version
        | None -> Lwt.return_error (SemverParserFailure version_str)
      in
      let version = NameVersion.SemverVersion version in
      let package = NameVersion.make ~name ~version in
      (match Closure.mem package closure with
      | true -> traverse rest closure cache
      | false ->
        let* metadata_json, cache =
          cached_get_metadata_json ~cache ~name ~version ~ligo_registry
        in
        let closure = Closure.add package closure in
        let* _ = Lwt.return @@ RegistryResponse.get_version (Ok metadata_json) in
        let rest = rest @ RegistryResponse.get_dependencies (Ok metadata_json) in
        traverse rest closure cache)
  in
  traverse dependencies Closure.empty Metadata_cache.empty


let generate_checksum ~(all_dependencies : NameVersion.t list) : string =
  let deps_to_string dependencies =
    List.sort ~compare:NameVersion.compare dependencies
    |> List.map ~f:NameVersion.to_string
    |> String.concat
  in
  deps_to_string all_dependencies |> Cli_helpers.Checksum.sha1


let solve ~ligo_registry ~all_dependencies =
  get_transitive_closure ~ligo_registry all_dependencies


let dependencies ligo_json solution =
  let deps =
    Json.Util.(member "dependencies" ligo_json |> to_assoc) |> List.unzip |> fst
  in
  let f name_version =
    let NameVersion.{ name; _ } = name_version in
    List.mem deps name ~equal:String.equal
  in
  let deps = List.filter ~f solution in
  deps


let dev_dependencies ligo_json solution =
  let deps = ligo_json |> get_dev_dep_list |> List.unzip |> fst in
  let f name_version =
    let NameVersion.{ name; _ } = name_version in
    List.mem deps name ~equal:String.equal
  in
  let deps = List.filter ~f solution in
  deps


let generate_lock_file
    ~(ligo_json : Json.t)
    ~(solution : NameVersion.t list)
    ~(project_name : string)
    ~(cache : Json.t Metadata_cache.t)
    : (Lock_file.t, error) Lwt_result.t
  =
  let dependencies = dependencies ligo_json solution in
  let dev_dependencies = dev_dependencies ligo_json solution in
  let metadata_json_list =
    let f name_version =
      Metadata_cache.find_opt name_version cache
      |> Result.of_option ~error:MetadataJsonCacheFail
    in
    List.map ~f solution |> Result.all
  in
  let nodes =
    let f lst = List.map ~f:generate_node lst |> Result.all in
    Result.map ~f metadata_json_list |> Result.join
  in
  let default_node =
    generate_default_node ~project_name ~ligo_json ~dependencies ~dev_dependencies
  in
  let root = Result.map ~f:(fun node -> node.id) default_node in
  let all_dependencies = dependencies @ dev_dependencies in
  let checksum = Ok (generate_checksum ~all_dependencies) in
  let node =
    let rec loop nodes acc =
      match nodes with
      | [] -> acc
      | node :: tl ->
        let Node.
              { id = _
              ; name
              ; version
              ; source = _
              ; overrides = _
              ; dependencies = _
              ; dev_dependencies = _
              }
          =
          node
        in
        (match version with
        | Node.NodeVersion version ->
          let version = NameVersion.SemverVersion version in
          let name_version = NameVersion.make ~name ~version in
          let acc = NodeMap.add name_version node acc in
          loop tl acc
        | Node.DefaultVersion _ -> loop tl acc)
    in
    let acc = NodeMap.empty in
    let f nodes =
      let nodes = loop nodes acc in
      Result.map default_node ~f:(fun default_node ->
          let Node.
                { id = _
                ; name
                ; version
                ; source = _
                ; overrides = _
                ; dependencies = _
                ; dev_dependencies = _
                }
            =
            default_node
          in
          let nodes =
            match version with
            | Node.DefaultVersion version ->
              let version = NameVersion.StringVersion version in
              let name_version = NameVersion.make ~name ~version in
              Ok (NodeMap.add name_version default_node nodes)
            | Node.NodeVersion version ->
              let version = NameVersion.SemverVersion version in
              let name_version = NameVersion.make ~name ~version in
              Ok (NodeMap.add name_version default_node nodes)
          in
          nodes)
    in
    Result.map ~f nodes |> Result.join |> Result.join
  in
  match checksum, root, node with
  | Ok checksum, Ok root, Ok node -> Lwt_result.return Lock_file.{ checksum; root; node }
  (* TODO : improve error checking by pattern matching more cases *)
  | _, Error msg, _ ->
    Lwt_result.fail (RootGenerationInLockFileFailed (string_of_error msg))
  | _, _, Error msg ->
    Lwt_result.fail (NodeGenerationInLockFileFailed (string_of_error msg))
  | _, _, _ -> Lwt_result.fail LockFileGenerationFailed


let write ~(content : string) ~(to_ : Fpath.t) =
  let filename = Filename.basename (Fpath.to_string to_) in
  let dirpath = Filename.chop_suffix (Fpath.to_string to_) filename in
  let create_dir = Bos.OS.Dir.create Fpath.(v dirpath) in
  let write_to_file = Bos.OS.File.write to_ content in
  match create_dir, write_to_file with
  | Ok _, Ok () -> Lwt_result.return ()
  | _, Error (`Msg msg) -> Lwt_result.fail (WriteFileFailed (Fpath.to_string to_, msg))
  | Error (`Msg msg), _ -> Lwt_result.fail (DirectoryCreationFailed msg)


let is_checksum_in_sync current_checksum all_dependencies =
  let latest_checksum = generate_checksum ~all_dependencies in
  String.equal
    current_checksum
    latest_checksum (* TODO replace with Digestif.SHA1.equal *)


let get_lock_file_that_is_in_sync ~project_root ~all_dependencies =
  let ( let+ ) = Caml.Option.bind in
  let+ lock_file_json =
    match read_lock_file ~project_root with
    | Ok json -> Some json
    | Error _ -> None
  in
  let checksum =
    Json.Util.(member "checksum" lock_file_json |> to_string) |> trim_quotes
  in
  let+ all_dependencies_nameVersion =
    let f acc tuple =
      match acc, nameVersion_of_tuple tuple with
      | Some acc, Ok nameVersion -> Some (nameVersion :: acc)
      | _, _ -> None
    in
    let init = Some [] in
    List.fold_left ~f ~init all_dependencies
  in
  (* Check if lock file is in sync with this list of dependencies *)
  match is_checksum_in_sync checksum all_dependencies_nameVersion with
  | true -> Some lock_file_json
  | false -> None


let rec update_kvs ~key ~value ~acc = function
  | [] -> acc
  | (k, v) :: rest ->
    let acc = if String.equal k key then (key, value) :: acc else (k, v) :: acc in
    update_kvs ~key ~value ~acc rest


(* Is meant to update a certain field in the json. This means, the given
   json has to be a key value pair object. Any other format cannot be updated *)
let update_json ~key ~value = function
  | `Assoc kvs -> Result.return @@ `Assoc (update_kvs ~key ~value ~acc:[] kvs)
  | _ -> Error (`Msg "update_json: Invalid destination format")


(* run `ligo install` *)
let run ~project_root package_name cache_path ligo_registry =
  let open Lwt_result.Syntax in
  let manifest_path = ligo_json project_root in
  let ligo_json = read_ligo_json ~project_root in
  let* dependencies = Lwt.return @@ field_to_tuple_list ~field:"dependencies" ligo_json in
  let* dependencies =
    match package_name with
    | Some package_name ->
      let* package_metadata =
        get_metadata_json ~name:package_name ~version:"*" ~ligo_registry
      in
      let latest_version =
        Json.Util.(package_metadata |> member "version" |> to_string)
      in
      (* TODO. It should have been a set *)
      let dependencies =
        match
          let f (n, v) = String.equal n package_name in
          List.find ~f dependencies
        with
        | Some _ -> dependencies
        | None -> (package_name, latest_version) :: dependencies
      in
      Lwt_result.return dependencies
    | None -> Lwt_result.return @@ dependencies
  in
  let* dev_dependencies =
    Lwt.return @@ field_to_tuple_list ~field:"devDependencies" ligo_json
  in
  let all_dependencies = dependencies @ dev_dependencies in
  match all_dependencies with
  | [] -> Lwt_result.return ()
  | _ ->
    let* solution, cache =
      match get_lock_file_that_is_in_sync ~project_root ~all_dependencies with
      | Some lock_file_json ->
        let* lock_file =
          Lwt.return
          @@
          match Lock_file.of_yojson lock_file_json with
          | Error m -> Error (LockFileParserFailure m)
          | Ok lock_file -> Ok lock_file
        in
        let solution = Lock_file.(NodeMap.bindings lock_file.node) |> List.map ~f:fst in
        Lwt_result.return (solution, Metadata_cache.empty)
      | None ->
        let* solution, cache = solve ~all_dependencies ~ligo_registry in
        remove_dir cache_path;
        let* lock_file_json =
          generate_lock_file ~project_name:"default-project" ~ligo_json ~solution ~cache
        in
        let* () =
          let content = Lock_file.to_yojson lock_file_json |> Json.to_string in
          write ~content ~to_:(lock_file project_root)
        in
        let* () =
          match package_name with
          | Some _ ->
            let f (k, v) = k, `String v in
            let dependencies_json = `Assoc (List.map ~f dependencies) in
            (match update_json ~key:"dependencies" ~value:dependencies_json ligo_json with
            | Ok json ->
              let content = Json.pretty_to_string json in
              write ~content ~to_:manifest_path
            | Error (`Msg m) -> Lwt_result.lift @@ Error (InternalError m))
          | None -> Lwt_result.return ()
        in
        Lwt_result.return (solution, cache)
    in
    let* installation_map =
      fetch_tarballs ~ligo_registry ~project_root ~cache ~solution
    in
    let installation_json_path = installation_json project_root in
    let installation_json =
      generate_installation_json installation_map project_root ~ligo_json
    in
    let* () =
      write ~content:(Json.to_string installation_json) ~to_:installation_json_path
    in
    Lwt_result.return ()
