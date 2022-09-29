module List = Core.List

let (let*) x f = Option.bind x f

type dependency_path = [`Path    of string]
type package         = [`Package of string]

type dependency_path_list  = dependency_path list
type package_list          = package list

type resolution = dependency_path * dependency_path_list

module PackageMap = Simple_utils.Map.Make (struct 
  type t = package
  
  let compare (`Package a) (`Package b) = String.compare a b 
end)

type dependency_map   = package_list    PackageMap.t
type installation_map = dependency_path PackageMap.t

type lock_file = {
  root : package ;
  node : package_list PackageMap.t
}

type t = {
  root_path   : dependency_path ;
  resolutions : resolution list ;
}

let traverse : 'a option list -> 'a list option 
  = List.fold_left ~init:(Some [])
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

  (* TODO: Try to have a single representation of path *)
  (* [join path1 path2] concatenates path1 / path2, It is aware of Fpath.dir_sep *)
  let join : string -> string -> string =
    fun a b ->
      match ends_with_dir_sep a, starts_with_dir_sep b with
      | false, false -> a ^ Fpath.dir_sep ^ b
      | false, true  
      | true , false -> a ^ b
      | true , true  -> a ^ "." ^ b

  (* [get_absolute_path path] returns the absolute path w.r.t. root *)
  let get_absolute_path ?(root=Sys.getcwd ()) path =
    let path' = v path in
    if is_abs path' then path'
    else v (join root path) 

  (* [to_string path] wrapper over Fpath.to_string *)
  let to_string path =
    let* path = path in
    Some (Fpath.to_string path)
end

(* Module with constants related esy *)
module Esy = struct 
  let ( / ) = Path.join
  
  (* Path to installation.json *)
  let installation_json_path path = 
    path / "_esy" / "default" / "installation.json"

  (* Path to lock file *)
  let lock_file_path path =
    path / "esy.lock" / "index.json"

  let path_separator = "__" 

  (* [extract_pkg_name path] extracts the package name from a path
     e.g. From a path like /path/to/ligo/cache/ligo__list_helpers__1.0.0__bf074147
     we want to extract the package name ligo__list_helpers in this case
     1. Get the basename                      (Path - ligo__list_helpers__1.0.0__bf074147     )
     2. We split the path by [path_separator] (Path - [ligo, list_helpers ; 1.0.0 ; bf074147] )
     3. Reverse the list                      (Path - [bf074147 ; 1.0.0 ; list_helpers ; ligo])
     4. Drop first 2 elements from list       (Path - [list_helpers ; ligo]                   )
     5. Reverse the list again                (Path - [ligo ; list_helpers]                   )
     6. Concat the string list with [path_separator] (Path - ligo__list_helpers               )*)
  let extract_pkg_name path =
    Filename.basename path
      |> Str.split (Str.regexp path_separator)
      |> List.rev
      |> fun xs -> List.drop xs 2
      |> List.rev
      |> String.concat path_separator

  (* [normalize_name name] transforms names used in esy paths, 
     1. '-' in the project name are transformed to '_' 
     2. '_' in the project name are transformed to '__' *)
  let normalize_name name = name
    |> String.split_on_char '_'
    |> String.concat "__"
    |> String.split_on_char '-' 
    |> String.concat "_"
     
end


(* [clean_installation_json] converts installation.json to a [installation_map]
   
  e.g. esy installation.json
  {
    "ligo-set-helpers@1.0.2@d41d8cd9":
      "/path/to/.esy/source/i/ligo_set_helpers__1.0.2__5cd724a1",
    "ligo-list-helpers@1.0.1@d41d8cd9":
      "/path/to/.esy/source/i/ligo_list_helpers__1.0.1__6233bebd",
    "ligo-main@link-dev:./esy.json":
      "/path/to/projects/ligo-pkg-mgmnt/ligo-main"
  } *)
let clean_installation_json abs_path_to_project_root installation_json =
  let open Yojson.Basic in
  let* installation_json = installation_json in
  let kvs = Util.to_assoc installation_json in
  List.fold_left
    kvs
    ~f:(fun m (key, value) ->
      let* m     = m in
      let  key   = `Package key in
      let* value = JsonHelpers.string value in
      let  abs_p = Path.get_absolute_path ~root:abs_path_to_project_root value in
      let* abs_s = Path.to_string abs_p in
      let  incl  = `Path abs_s in
      Some (PackageMap.add key incl m)
    ) 
    ~init:(Some PackageMap.empty)

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
          "ligo_test_1@1.0.0@d41d8cd9",
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
  record [lock_file] *)
let clean_lock_file_json lock_json =
  let* lock_json = lock_json in
    let open Yojson.Basic in
    let* root = Util.member "root" lock_json |> JsonHelpers.string in
    let  root = `Package root in
    let  node = Util.member "node" lock_json in
    let  kvs  = Util.to_assoc node in
    let* node = List.fold_left
      kvs
      ~f:(fun m (key, value) ->
        let* m = m in
        let key = `Package key in
        let  dependencies = Util.member "dependencies" value in  
        let* dependencies = JsonHelpers.string_list dependencies in
        let  dependencies = List.map dependencies ~f:(fun d -> `Package d) in
        Some (PackageMap.add key dependencies m)
      )
      ~init:(Some PackageMap.empty) in
    Some ({ root ; node })

(* [resolve_path] looks up the path of a package and returns its absolute
   path w.r.t. root of the project *)
let resolve_path abs_path_to_project_root installation pkg_name = 
  let* (`Path path) = PackageMap.find_opt pkg_name installation in
  let  path = Path.get_absolute_path ~root:abs_path_to_project_root path in
  let* path = Path.to_string path in
  Some path 

(* [resolve_paths] resolves the the package names into file system paths *)
let resolve_paths abs_path_to_project_root installation graph : resolution list option =
  PackageMap.fold (fun k v xs ->
    let* xs = xs in
    let* resolved = traverse (List.map v ~f:(resolve_path abs_path_to_project_root installation)) in
    let* k = resolve_path abs_path_to_project_root installation k in
    let  k = `Path k in
    let paths = List.sort ~compare:String.compare resolved in 
    let paths = List.map ~f:(fun p -> `Path p) paths in
    Some ((k, paths) :: xs)
  ) graph (Some [])

(* [find_dependencies] takes the lock file and traverses the dependency
   graph and constructs a Map of package_name as key and list of dependencies
   of the package as value *)
let find_dependencies (lock_file : lock_file) : package_list PackageMap.t = 
  let { root ; node } = lock_file in
  let rec dfs graph dep =
    if PackageMap.mem dep graph 
    then graph 
    else
      let deps = PackageMap.find_opt dep node in
      match deps with
        Some deps -> 
          let graph = PackageMap.add dep deps graph in
          List.fold_left deps ~init:graph ~f:dfs 
      | _ -> graph
  in
  dfs PackageMap.empty root 

(* [make] takes the root of LIGO project and locates the intallation.json 
   & lock file and constructs a data structure which will be used to
   look up paths to LIGO packages.

   [make] combines the data in the installation.json & lock file
   using the [find_dependencies] & [resolve_paths] functions into a uniform
   representation *)
let make project_root : t option =
  let  abs_path_to_project_root = Path.get_absolute_path project_root in
  let* abs_path_to_project_root = Path.to_string abs_path_to_project_root in
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
  let  root_path   = `Path root_path in
  Some { root_path ; resolutions }

(* [get_paths_of_root_dependencies] is used in the case when external dependencies are
   used in the REPL using the #use or #import commands & Test.originate_from_file.
  
   this functions gives the list of paths of dependencies used by the main project *)
let get_paths_of_root_dependencies =
  function 
  | Some {root_path;resolutions;_} ->
    let root_inclusion_list = List.find resolutions
      ~f:(fun (path,_) -> path = root_path) in
    (match root_inclusion_list with 
      Some (_,root_inclusion_list) -> root_inclusion_list
    | None -> [])
  | None -> []

(* [get_paths_of_dependencies ~file module_resolutions] 
    returns the inclusion list (list of paths
    of dependencies of that project/dependency) 
   
   e.g. #include "ligo-list-helpers/list.mligo" 
   The preprocessor will resolve the include into a path like 
   /path/to/ligo_list_helpers/list.mligo

   To resolve the external packages used by ligo-list-helpers [get_paths_of_dependencies]
   will give a list of paths of dependencies. *)
let get_paths_of_dependencies ~file =
  function
    Some {resolutions;_} ->
      let path = Path.get_absolute_path file in
      let resolution = List.find resolutions ~f:(fun (`Path mod_path, _) ->
        let mod_path = Path.v mod_path in
        Path.is_prefix mod_path path) 
      in 
      (match resolution with
        Some (_,paths) -> paths
      | None -> []
      )
  | None -> []

(* [find_in_inclusion_list ~inclusion_list ?scope_name pkg_name] normalizes the
   [pkg_name] & [scope_name] and tries to find the path in the inclusion list
   which starts with the same normalized name.
   [Esy.extract_pkg_name path] helps to find the the normalized package name
    in the [path] *)
let find_in_inclusion_list ~inclusion_list ?scope_name pkg_name =
  let normalized_pkg_name = Esy.normalize_name pkg_name in
  let normalized_pkg_name = 
    match scope_name with
      Some scope_name ->
        (* esy stores scoped packages in its cache as 
            `${normalized_scope}__s__${normalized_pkg}`
            e.g. for a scoped pkg like @x-y/somepkg of version 1.2.3
                it will be stored as `x_y__s__somepkg__1.2.3__${hash}` *)
        let normalized_scope_name = Esy.normalize_name scope_name in
        Format.sprintf "%s__s__%s" normalized_scope_name normalized_pkg_name
    | None -> normalized_pkg_name in
  List.find inclusion_list ~f:(fun (`Path pkg_path) ->
    normalized_pkg_name = Esy.extract_pkg_name pkg_path)

(* [find_external_file ~file ~inclusion_list] specifically resolves files
   for LIGO packages downloaded via esy.

   The [inclusion_list] contains a list of paths of the form
   {package-name}__{version}__{hash}
   e.g. /path/to/ligo/cache/ligo_list_helpers__1.0.0__bf074147

   a ligo package will be used in #import or #include,
   e.g. #import "ligo-list-helpers/list.mligo" "ListHelpers"

   To correctly resolve the path for #import or #include we split the 
   path into 2 parts i.e. the package name & rest of the path
   e.g. "ligo-list-helpers/list.mligo" - 
    package name = ligo-list-helpers
    rest of path = list.mligo
  
   Then we look for a path corresponding to package name in the [inclusion_list] *)
let find_external_file ~file ~inclusion_list =
  let* segs = Path.segs (Path.v file) in
  let* (`Path dir, rest_of_path) =
    match segs with
      scope::pkg_name::rest_of_path 
        when Core.String.is_prefix ~prefix:"@" scope ->
        (* scoped npm packages are of the form `@scope/pkg` *)
        let scope_name = Core.String.chop_prefix_exn ~prefix:"@" scope in
        let path_opt = find_in_inclusion_list ~inclusion_list ~scope_name pkg_name in
        Option.map (fun path -> path, rest_of_path) path_opt
    | pkg_name::[] -> None
    | pkg_name::rest_of_path -> 
        let path_opt = find_in_inclusion_list ~inclusion_list pkg_name in
        Option.map (fun path -> path, rest_of_path) path_opt
    | _ -> None
  in
  let path = List.fold_left ~f:Path.join ~init:dir rest_of_path in
  Some path

(* [pp] pretty-printing for module resolutions *)
let pp ppf mr =
  let { root_path ; resolutions } = mr in
  let (`Path root_path) = root_path in
  let () = Format.fprintf ppf "Root Path = %s\n" root_path in
  let () = Format.fprintf ppf "Resolutions =\n" in
  let pp_inclusion_list ppf is = 
    List.iter is ~f:(fun (`Path i) -> Format.fprintf ppf "\t%s\n" i) in
  List.iter resolutions 
    ~f:(fun ((`Path k), v) -> 
          Format.fprintf ppf "%s = [\n%a\n]\n" k pp_inclusion_list v)

module Helpers = struct
  (* [resolve_file_name file_name module_resolutions] is a helper used by REPL 
     & Test.originate_from_file to get the path of LIGO package 
     e.g. In the REPL `#use 'ligo-list-helpers/list.mligo';;`
     the pathe to #use will be resolved using this function, which internally
     calls the functions [get_paths_of_root_dependencies] & [find_external_file] *)
  let resolve_file_name file_name module_resolutions =
    if Stdlib.Sys.file_exists file_name then file_name
    else
      let inclusion_list = get_paths_of_root_dependencies module_resolutions in
      let external_file  = find_external_file ~file:file_name ~inclusion_list in
      (match external_file with
        Some external_file -> external_file
      | None -> file_name)
end