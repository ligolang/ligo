let (let*) x f    = Option.bind x f

type inclusion_dir = [`Inclusion of string]
type dependency = [`Dependency of string]
type inclusion_list = inclusion_dir list
type dependency_list = dependency list


module DepMap = Simple_utils.Map.Make (struct 
  type t = dependency
  
  let compare (`Dependency a) (`Dependency b) = String.compare a b 
end)

type dependency_map = dependency_list DepMap.t

type installation_map = inclusion_list DepMap.t


(* module List = Core.List *)

type lock_file = {
  root : dependency ;
  node : dependency_list DepMap.t
}

type t = {
  root_path   : inclusion_dir ;
  resolutions : (inclusion_dir * inclusion_list) list ;
}

let traverse : 'a option list -> 'a list option 
  = 
  let module List = Core.List in
  List.fold_left
      ~init:(Some [])
      ~f:(
        fun acc x ->
          let* acc = acc in
          let* x = x in
          Some (x :: acc)
      )

(* Wrapper over yojson helpers *)
module JsonHelpers = struct
  (* [string] tries to extract the `String contents from Yojson.t *)
  let string (json : Yojson.Basic.t) = 
    match json with
      `String s -> Some s
    | _         -> None

  (* [list] tries to extract the `List from Yojson.t *)
  let list (json : Yojson.Basic.t) = 
    match json with
      `List l -> Some l
    | _       -> None

  (* [string_list] tries to extract the `List [`String _] from Yojson.t *)
  let string_list (json : Yojson.Basic.t) = 
    let l = list json in
    match l with
      Some l ->
        let module List = Core.List in
        let strings = List.map ~f:string l in
        traverse strings
    | None -> None

  (* [from_file_opt] tries to read the file as json (Yojson.t) *)
  let from_file_opt (file : string) : Yojson.Basic.t option =
    try Some (Yojson.Basic.from_file file) with _ -> None
end

(* Wrapper over Fpath that does not raise exceptions *)
module Path = struct
  type t = Fpath.t option

  (* function for string to Path.t *)
  let v : string -> t = fun s -> try Some (Fpath.v s |> Fpath.normalize) with _ -> None

  (* alias to Filename.dir_sep *)
  let dir_sep = Filename.dir_sep

  (* [segs] splits the segments in a path 
     e.g /a/b/c.ligo -> ["a";"b";"c.ligo"] *)
  let segs : t -> string list option = fun t -> Option.map Fpath.segs t

  (* [is_prefix] checks if [prefix] is a prefix of [p] 
     eg. prefix = /a/b/c/ , p = /a/b/c/d/e.ligo 
         is_prefix prefix p = true *)
  let is_prefix : t -> t -> bool = 
    fun prefix p ->
      match prefix, p with
        Some prefix, Some p -> 
          Fpath.is_prefix prefix p
      | _ -> false 
  
  (* [is_abs] checks if path is absolute path *)
  let is_abs : t -> bool = function Some p -> Fpath.is_abs p | None -> false

  (* [normalize] gets rid of ".." & "." from Path.t *)
  let normalize : t -> t = Option.map Fpath.normalize

  (* [ends_with_dir_sep path] checks if [path] ends with [Fpath.dir_sep] *)
  let ends_with_dir_sep =
    let regexp = Str.regexp (Format.sprintf "%s$" Fpath.dir_sep) in
    fun path -> Str.string_match regexp path 0

  (* [starts_with_dir_sep path] checks if [path] starts with [Fpath.dir_sep] *)
  let starts_with_dir_sep =
    let regexp = Str.regexp (Format.sprintf "^%s" Fpath.dir_sep) in
    fun path -> Str.string_match regexp path 0

  (* [join path1 path2] concatenates path1 / path2, It is aware of Fpath.dir_sep *)
  let join : string -> string -> string =
    fun a b ->
      match ends_with_dir_sep a, starts_with_dir_sep b with
      | false, false -> a ^ Fpath.dir_sep ^ b
      | false, true  
      | true , false -> a ^ b
      | true , true  -> a ^ "." ^ b (* TODO: fix this case ... *)

  let get_absolute_path path =

    let path' = v path in
    if is_abs path' then path'
    else
      let cwd = Sys.getcwd () in
      v (join cwd path) |> normalize 

end

(* Module with constants related esy *)
module Esy = struct 
  let ( / ) = Path.join
  
  let installation_json_path path = 
    path / "_esy" / "default" / "installation.json"

  let lock_file_path path =
    path / "esy.lock" / "index.json"

  let path_separator = "__" 

  (* TODO: docs
    why??
    
  *)
  let extract_pkg_name path =
    let module List = Core.List in
    Filename.basename path
      |> Str.split (Str.regexp path_separator)
      |> List.rev
      |> fun xs -> List.drop xs 2
      |> List.rev
      |> String.concat path_separator

  (*    
    TODO: docs
   Esy transforms the path of package names, '-' in the project name are transformed to '_' 
   & '_' in the project name are transformed to '__'
   ...
 *)
  let normalize_pkg_name pkg_name = pkg_name
    |> String.split_on_char '_'
    |> String.concat "__"
    |> String.split_on_char '-' 
    |> String.concat "_"
     
end


(* [clean_installation_json] converts installation.json to a string SMap.t option
   
  e.g. esy installation.json
  {
    "ligo-set-helpers@1.0.2@d41d8cd9":
      "/path/to/.esy/source/i/ligo_set_helpers__1.0.2__5cd724a1",
    "ligo-list-helpers@1.0.1@d41d8cd9":
      "/path/to/.esy/source/i/ligo_list_helpers__1.0.1__6233bebd",
    "ligo-main@link-dev:./esy.json":
      "/path/to/projects/ligo-pkg-mgmnt/ligo-main"
  }
   
*)
let clean_installation_json abs_path_to_project_root installation_json =
  let open Yojson.Basic in
  let* installation_json = installation_json in
  let keys   = Util.keys   installation_json in
  let values = Util.values installation_json in
  List.fold_left2
    (fun m key value ->
      let* m     = m in
      let  key   = `Dependency key in
      let* value = JsonHelpers.string value in
      let* value = if Path.is_abs (Path.v value) then (Path.v value) else Path.v (Path.join abs_path_to_project_root value) in
      let  value = Fpath.to_string value in
      let  value = `Inclusion value in
      Some (DepMap.add key value m)
    ) 
    (Some DepMap.empty) 
    keys values

(* [clean_lock_file_json] converts the esy lock file to a record of type lock_file 

  e.g. esy lock file
  {
    "checksum": "<some hash>"
    "root": "ligo-main@link-dev:./esy.json",
    "node": {
      "ligo-main@link-dev:./esy.json": {
        "id": "ligo-main@link-dev:./esy.json",
        "name": "ligo-main",
        "version": "link-dev:./esy.json",
        "source": { "type": "link-dev", "path": ".", "manifest": "esy.json" },
        "overrides": [],
        "dependencies": [
          "temp-ligo-bin@0.30.1@d41d8cd9", "ligo_test_1@1.0.0@d41d8cd9",
          "ligo-test_2@1.0.0@d41d8cd9", "ligo-list-helpers@1.0.1@d41d8cd9",
          "ligo-foo@1.0.5@d41d8cd9"
        ],
        "devDependencies": []
      },
      ...
    }
  }

  For each dependency in inside "node" we are only interested in the "dependencies"
  field, we extract the necessary fields from the esy lock json and construct the 
  record lock_file { root : string ; node : dependency_list SMap.t } 
  
  in this case 
  { 
    root = "ligo-main@link-dev:./esy.json" ;
    node = SMap.t ({ "ligo-main@link-dev:./esy.json" = [
      "temp-ligo-bin@0.30.1@d41d8cd9", 
      "ligo_test_1@1.0.0@d41d8cd9",
      "ligo-test_2@1.0.0@d41d8cd9", 
      "ligo-list-helpers@1.0.1@d41d8cd9",
      "ligo-foo@1.0.5@d41d8cd9"
    ]; ... })
  } *)
let clean_lock_file_json lock_json =
  let* lock_json = lock_json in
    let open Yojson.Basic in
    let* root = Util.member "root" lock_json |> JsonHelpers.string in
    let  root = `Dependency root in
    let  node = Util.member "node" lock_json in
    let keys   = Util.keys   node in
    let values = Util.values node in
    let* node = List.fold_left2
      (fun m key value ->
        let* m = m in
        let key = `Dependency key in
        let  dependencies = Util.member "dependencies" value in  
        let* dependencies = JsonHelpers.string_list dependencies in
        let dependencies = List.map (fun d -> `Dependency d) dependencies in
        Some (DepMap.add key dependencies m)
      )
      (Some DepMap.empty)
      keys values in
    Some ({ root ; node })

let resolve_path abs_path_to_project_root installation pkg_name = 
  let* (`Inclusion path) = DepMap.find_opt pkg_name installation in
  let* path = if Path.is_abs (Path.v path) then Path.v path else Path.v (Path.join abs_path_to_project_root path) in
  let  path = Fpath.to_string path in
  (* let () = print_endline path in *)
  Some path 

(* [resolve_paths] takes the installation.json {string SMap.t} and 
   the Map constructed by [find_dependencies] and resolves the the
   package names into file system paths *)
let resolve_paths abs_path_to_project_root installation (graph : dependency_map) : (inclusion_dir * (inclusion_list)) list option =
  DepMap.fold (fun k v xs ->
    let module List = Core.List in
    let* xs = xs in
    let* resolved = traverse (List.map v ~f:(resolve_path abs_path_to_project_root installation)) in
    let* k = resolve_path abs_path_to_project_root installation k in
    let  k = `Inclusion k in
    let paths = List.sort ~compare:String.compare resolved in 
    let paths = List.map ~f:(fun p -> `Inclusion p) paths in
    Some ((k, paths) :: xs)
  ) graph (Some [])

(* [find_dependencies] takes the esy lock file and traverses the dependency
   graph and constructs a Map of package_name as key and list of dependencies
   of the package as value *)
let find_dependencies (lock_file : lock_file) : dependency_list DepMap.t = 
  let root = lock_file.root in
  let node = lock_file.node in
  let rec dfs dep graph =
    if DepMap.mem dep graph then graph else
    let deps = DepMap.find_opt dep node in
    match deps with
      Some deps -> 
        let module List = Core.List in
        let graph = DepMap.add dep deps graph in
        List.fold_left deps ~init:graph ~f:(fun graph dep -> dfs dep graph)
    | _ -> graph
  in
  dfs root DepMap.empty

(* [make] takes the root of an esy project and locates the 
   esy intallation.json & esy lock file and constructs a record of 
   { root_path : string ; resolutions : (string * inclusion_list) list }

   [make] combines the data in the esy installation.json & esy lock file
   using the [find_dependencies] & [resolve_paths] functions into a uniform
   representation *)
let make project_root : t option =
  let* abs_path_to_project_root = Path.get_absolute_path project_root in
  let  abs_path_to_project_root = Fpath.to_string abs_path_to_project_root in
  (* let () = print_endline abs_path_to_project_root in *)
  let* installation_json = Esy.installation_json_path project_root 
    |> JsonHelpers.from_file_opt
    |> clean_installation_json abs_path_to_project_root
  in
  let* lock_file_json = Esy.lock_file_path project_root 
    |> JsonHelpers.from_file_opt
    |> clean_lock_file_json
  in
  let root = lock_file_json.root in
  let dependencies = find_dependencies lock_file_json in
  let* resolutions = resolve_paths abs_path_to_project_root installation_json dependencies in
  let* root_path   = resolve_path  abs_path_to_project_root installation_json root in
  let  root_path   = `Inclusion root_path in
  Some { root_path ; resolutions }

(* [get_root_inclusion_list] is used in the case when external dependencies are
   used in the REPL using the #use or #import commands.
  
   this functions gives the list of paths of dependencies used by the main project
*)
let get_root_inclusion_list =
  function 
  | Some module_resolutions ->
    let module List = Core.List in
    let root_path = module_resolutions.root_path in
    let root_inclusion_list = List.find module_resolutions.resolutions
      ~f:(fun (path,_) -> 
        (* let () = Format.printf "%s - %s\n" path root_path in *)
        path = root_path) 
      in
    (match root_inclusion_list with 
      Some (_,root_inclusion_list) -> root_inclusion_list
    | None -> [])
  | None -> []

(* [get_inclusion_list ~file module_resolutions] 
    returns the inclusion list (list of paths
    of dependencies of that project/dependency) 
   
   e.g. #include "ligo-list-helpers/list.mligo" 
   The preprocessor will resolve the include into a path like 
   /path/to/ligo_list_helpers/list.mligo

   To resolve the external packages used by ligo-list-helpers [get_inclusion_list]
   will give a list of paths of dependencies. *)
let get_inclusion_list ~file =
  function
    Some module_resolutions ->
      let module List = Core.List in
      let path = Path.get_absolute_path file in
      (match List.find module_resolutions.resolutions ~f:(fun (`Inclusion mod_path, _) ->
        (* let () = Format.printf "%s - %s\n" mod_path file in *)
        let mod_path = Path.v mod_path in
        Path.is_prefix mod_path path (* we need is_prefix because path is of the form abs_path/lib/path/to/file and mod_path is of the form abs_path*)
        ) 
      with
        Some (_,paths) -> paths
      | None -> []
      )
  | None -> []

(* [find_external_file] specifically resolves files
   for ligo packages downloaded via esy.

   the [inclusion_list] contains a list of paths. 
   esy package path/dir is of the form {package-name}__{version}__{hash}
   e.g. /path/to/esy/cache/ligo_list_helpers__1.0.0__bf074147

   a ligo package will be used in #import or #include,
   e.g. #import "ligo-list-helpers/list.mligo" "ListHelpers"

   To correctly resolve the path for #import or #include we split the 
   path into 2 parts i.e. the package name & rest of the path
   e.g. "ligo-list-helpers/list.mligo" - 
    package name = ligo-list-helpers
    rest of path = list.mligo
*)
let find_external_file ~file ~inclusion_list =
  let module List = Core.List in
  let find_in_inclusion_list pkg_name = 
    let normalized_pkg_name = Esy.normalize_pkg_name pkg_name in
    List.find inclusion_list ~f:(fun (`Inclusion pkg_path) ->
      normalized_pkg_name = Esy.extract_pkg_name pkg_path)
  in
  let* segs = Path.segs (Path.v file) in
  let* segs =
    match segs with
      pkg_name::rest_of_path -> 
        let* (`Inclusion dir) = find_in_inclusion_list pkg_name in
        let rest_of_path = String.concat Filename.dir_sep rest_of_path in
        let dir = Path.join dir rest_of_path in
        Some dir
    | _ -> None
  in
  Some segs

(* TODO: pp for ModRes.t *)
(* TODO: Docs update *)
(* TODO: Dont mix Fpath & Path *)
