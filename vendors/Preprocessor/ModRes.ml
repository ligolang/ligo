(* Module for resolving paths to external LIGO libraries (packages) *)

(* Utilities *)

module SMap = Caml.Map.Make (String)

(* Monadic bind *)

let (let*) x f = Option.bind x ~f

(* Dependencies as file paths *)

type dependency_path = Path of string

let equal_paths (Path p1) (Path p2) = String.(p1 = p2)

(* TODO Explain the following types. *)

type resolution = dependency_path * dependency_path list

type t = {
  root_path   : dependency_path;
  resolutions : resolution list
}

(* Packages *)

type package = Package of string

module PackageOrd =
  struct
    type t = package

    let compare (Package a) (Package b) = String.compare a b
  end

module PackageMap = Caml.Map.Make (PackageOrd)

(* TODO Explain the following types. *)

type dependency_map   = package list    PackageMap.t
type installation_map = dependency_path PackageMap.t

type lock_file = {
  root : package;
  node : package list PackageMap.t
}

(* Utilities *)

let compact : 'a option list -> 'a list option =
  let folded x acc =
    let* acc = acc in
    let* x   = x in
    Some (x :: acc)
  in List.fold_right ~f:folded ~init:(Some [])

(* Wrapper over Yojson helpers *)

module JsonHelpers =
  struct
    let string_of (json : Yojson.Basic.t) =
      match json with
        `String s -> Some s
      | _         -> None

    let string_list_of (json : Yojson.Basic.t) =
      match json with
        `List l -> compact @@ List.map ~f:string_of l
      | _ -> None

    (* The function [from_file_opt] tries to read a file, assuming it
       contains text formatted as JSON. In case of success, it
       produces a value of type [Yojson.t] corresponding to the
       contents of the input file. (We use optional values to account
       for failures.) *)

    let from_file_opt (file : string) : Yojson.Basic.t option =
      try Some (Yojson.Basic.from_file file) with _ -> None
  end

(* The module [Path] is a wrapper over module [Fpath]. One difference
   is that functions of [Path] do not raise exceptions. Also, we work
   with optional paths instead of paths directly. We keep the same
   names for the functions found in [Fpath]. *)

module Path =
  struct
    type t = Fpath.t option

    (* The function [v] makes a value of type [Path.t] from a
       string. *)

    let v : string -> t =
      fun s -> 
        try Some (Fpath.v s |> Fpath.normalize |> Fpath.rem_empty_seg) 
        with _ -> None

    (* Alias of [Filename.dir_sep]. *)

    (* The function [segs] splits a file path into segments,
       e.g. "/a/b/c.ligo" yields [["a";"b";"c.ligo"]]. *)

    let segs : t -> string list option = Option.map ~f:Fpath.segs

    (* The predicate [is_abs] checks if a path is absolute. *)

    let is_abs : t -> bool =
      function Some p -> Fpath.is_abs p | None -> false

    (* The function [normalize] gets rid of [".."] and ["."] from
       [Path.t] *)

    let normalize : t -> t = Option.map ~f:Fpath.normalize

    (* The predicate [ends_with_dir_sep] checks if a given file path
       [path] ends with the directory separator [Fpath.dir_sep]. *)

    let ends_with_dir_sep =
      let regexp = Str.regexp (Format.sprintf "%s$" Fpath.dir_sep)
      in fun path -> Str.string_match regexp path 0

    (* The predicate [starts_with_dir_sep] checks if a given file path
       [path] starts with the directory separator [Fpath.dir_sep]. *)

    let starts_with_dir_sep =
      let regexp = Str.regexp (Format.sprintf "^%s" Fpath.dir_sep)
      in fun path -> Str.string_match regexp path 0

    (* The call [join path1 path2] concatenates file paths [path1] and
       [path2], assuming that the directory separator is
       [Fpath.dir_sep]. *)

    (* TODO: Define a canonical representation for paths. *)

    let join : string -> string -> string =
      fun a b ->
        match ends_with_dir_sep a, starts_with_dir_sep b with
          false, false -> a ^ Fpath.dir_sep ^ b
        | false, true
        | true , false -> a ^ b
        | true , true  -> a ^ "." ^ b

    (* [get_absolute_path path] returns the absolute path
       w.r.t. the root *)

    let get_absolute_path ?(root=Sys_unix.getcwd ()) path =
      let path' = v path in
      if is_abs path' then path' else v (join root path)

    (* The function [to_string] is a wrapper over
       [Fpath.to_string]. *)

    let to_string path =
      let* path = path in Some (Fpath.to_string path)

    (* The function [dirpath] drops the last segment from a path
       e.g. "/a/b/c.mligo" yields "/a/b" *)

    let dirpath path = 
      path
    |> Option.map ~f:Fpath.split_base
    |> Option.map ~f:fst
    |> Option.map ~f:Fpath.rem_empty_seg

    (* The predicate [equal] checks if the two given path [p1] & [p2]
       are equal *)

    let equal p1 p2 =
      match p1, p2 with
        Some p1, Some p2 -> 
          Fpath.equal (Fpath.normalize p1) (Fpath.normalize p2)
      | _ -> false

    (* The predicate [is_root] checks in the given path is the root 
       directory *)

    let is_root = function None -> false | Some p -> Fpath.is_root p

  end

(* Module with constants related esy *)

(* TODO: Create a functor with the signature of the module Esy *)

module Esy =
  struct
    let ( / ) = Path.join

    (* Path to installation.json *)

    let installation_json_path path =
      path / "_esy" / "default" / "installation.json"

    (* Path to the lock file *)

    let lock_file_path path = path / "esy.lock" / "index.json"

    (* Path separator for Esy *)

    let path_separator = "__"

    (* The function [extract_pkg_name] extracts the package name from a
       path. For example, given the path

       ["/path/to/ligo/cache/ligo__list_helpers__1.0.0__bf074147"],

       this function extracts the package name ligo__list_helpers.
       In this case, the following steps are taken:

         1. We get the basename:
            ["ligo__list_helpers__1.0.0__bf074147"];

         2. We split the path at occurrences of [path_separator]:
            [["ligo"; "list_helpers"; "1.0.0"; "bf074147"]];

         3. The list is reversed:
            [["bf074147"; "1.0.0"; "list_helpers"; "ligo"]];

         4. We drop the first two elements from the list:
            [["list_helpers"; "ligo"]];

         5. The list is reversed again:
            [["ligo"; "list_helpers"]];

         6. We concatenate the list of strings, with the separator
            [path_separator]:
            ["ligo__list_helpers"]. *)

    let extract_pkg_name path : string =
        Filename.basename path
      |> Str.split (Str.regexp path_separator)
      |> List.rev
      |> fun xs -> List.drop xs 2
      |> List.rev
      |> String.concat ~sep:path_separator

    (* The function [normalize_pkg_name] produces the canonical form
       of a package name by the following mangling rules:

         1. '-' in the name are transformed to '_';

         2. '_' in the name are transformed to '__' *)

    let normalize_pkg_name pkg_name =
      pkg_name
    |> String.split_on_chars ~on:['_']
    |> String.concat ~sep:"__"
    |> String.split_on_chars ~on:['-']
    |> String.concat ~sep:"_"

    let normalize_pkg_name ?scope pkg_name =
      let pkg_name = normalize_pkg_name pkg_name in
      match scope with
        Some scope ->
          let scope = normalize_pkg_name scope in
          Format.sprintf "%s__s__%s" scope pkg_name
      | None -> pkg_name

  end

(* The function [clean_installation_json] reads the JSON installation
   file (usually ["installation.json"]) and produces a value of type
   [string SMap.t option]. The absolute path to the root of the
   project is needed. For example, the call [clean_installation_json
   installation.json] yields the following JSON:

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
  let* installation_json = installation_json in
  let kvs = Yojson.Basic.Util.to_assoc installation_json in
  let folded map (key, value) =
    let* map   = map in
    let  key   = Package key in
    let* value = JsonHelpers.string_of value in
    let  abs_p = Path.get_absolute_path ~root:abs_path_to_project_root value in
    let* abs_s = Path.to_string abs_p in
    let  incl  = Path abs_s in
    Some (PackageMap.add key incl map)
  in List.fold_left kvs ~f:folded ~init:(Some PackageMap.empty)

(* The function [clean_lock_file_json] converts and esy lock file to a
   record of type [lock_file]. For example, here is an esy lock file:

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

  For each dependency inside [node] we are only interested in the
  [dependencies] field, and we extract the necessary fields from the
  esy lock file (interpreted a JSON) to construct the record of type
  [lock_file], that is, [{root : string; node : dependency_list
  SMap.t}].

  For example, resuming the example above yields:

  [{
    root = "ligo-main@link-dev:./esy.json" ;
    node = SMap.t ({ "ligo-main@link-dev:./esy.json" = [
      "temp-ligo-bin@0.30.1@d41d8cd9",
      "ligo_test_1@1.0.0@d41d8cd9",
      "ligo-test_2@1.0.0@d41d8cd9",
      "ligo-list-helpers@1.0.1@d41d8cd9",
      "ligo-foo@1.0.5@d41d8cd9"
    ]; ... })
  }] *)

let clean_lock_file_json lock_json =
  let* lock_json = lock_json in
  let  module Util = Yojson.Basic.Util in
  let* root = Util.member "root" lock_json |> JsonHelpers.string_of in
  let  root = Package root in
  let  node = Util.member "node" lock_json in
  let  kvs  = Util.to_assoc node in
  let  folded map (key, value) =
    let* map = map in
    let  key = Package key in
    let  dependencies = Util.member "dependencies" value in
    let* dependencies = JsonHelpers.string_list_of dependencies in
    let  dependencies = List.map dependencies ~f:(fun d -> Package d)
    in Some (PackageMap.add key dependencies map) in
  let* node = List.fold_left kvs ~f:folded ~init:(Some PackageMap.empty)
  in Some {root; node}

(* The function [resolve_path] looks up the path of a package and
   returns its absolute path w.r.t. root of the project (which is
   given). *)

let resolve_path abs_path_to_project_root installation pkg_name =
  let* Path path = PackageMap.find_opt pkg_name installation in
  let  path = Path.get_absolute_path ~root:abs_path_to_project_root path in
  let* path = Path.to_string path
  in Some path

(* The function [resolve_paths] takes a value of type [string SMapt.]
   denoting the contents of the file "installation.json", and the map
   constructed by [find_dependencies]. It then resolves the package
   names into file system paths (as a list). *)

let resolve_paths abs_path_to_project_root installation graph
    : resolution list option =
  let folded key value entries =
    let* entries  = entries in
    let  mapped   = resolve_path abs_path_to_project_root installation in
    let* resolved = compact @@ List.map value ~f:mapped in
    let* key   = resolve_path abs_path_to_project_root installation key in
    let  key   = Path key in
    let  paths = List.sort ~compare:String.compare resolved in
    let  paths = List.map ~f:(fun p -> Path p) paths
    in Some ((key, paths) :: entries)
  in PackageMap.fold folded graph (Some [])

(* The function [find_dependencies] takes the esy lock file and
   traverses the dependency graph, and constructs a map of package
   names (as keys), each associated to a list of packages )as
   values). *)

let find_dependencies (lock_file : lock_file) : package list PackageMap.t =
  let {root; node} = lock_file in
  let rec dfs graph dep =
    if PackageMap.mem dep graph
    then graph
    else
      let deps = PackageMap.find_opt dep node in
      match deps with
        Some deps ->
          let graph = PackageMap.add dep deps graph
          in List.fold_left deps ~init:graph ~f:dfs
      | _ -> graph
  in dfs PackageMap.empty root

(* The function [make] takes the root of LIGO project and locates both
   the "intallation.json" file and the lock file, then constructs a
   data structure which will be used to look up paths to LIGO
   packages. The function [make] combines the contents of
   "installation.json" and the lock file using the functions
   [find_dependencies] and [resolve_paths]. *)

let make project_root : t option =
  let  abs_path_to_project_root = Path.get_absolute_path project_root in
  let* abs_path_to_project_root = Path.to_string abs_path_to_project_root in
  let* installation_json =
    Esy.installation_json_path project_root
    |> JsonHelpers.from_file_opt
    |> clean_installation_json abs_path_to_project_root in
  let* lock_file_json =
    Esy.lock_file_path project_root
    |> JsonHelpers.from_file_opt
    |> clean_lock_file_json
  in
  let  root = lock_file_json.root in
  let  dependencies = find_dependencies lock_file_json in
  let* resolutions =
    resolve_paths abs_path_to_project_root installation_json dependencies in
  let* root_path =
    resolve_path  abs_path_to_project_root installation_json root in
  let  root_path = Path root_path
  in Some {root_path; resolutions}

(* The function [get_root_dependencies] is used in the case when
   external dependencies are used in the REPL using the #use or
   #import commands & Test.originate_from_file.

   This functions gives the list of paths of dependencies used by the
   main project *)

let get_root_dependencies = function
  None -> []
| Some {root_path; resolutions} ->
    let root_inclusion_paths =
      List.find resolutions ~f:(fun (p,_) -> equal_paths p root_path)
    in match root_inclusion_paths with
         Some (_, paths) -> paths
       | None -> []

(* The function [get_dependencies ~file module_resolutions] returns
   the inclusion list (list of paths of dependencies of that
   project/dependency)

   e.g. #include "ligo-list-helpers/list.mligo"
   The preprocessor will resolve the #include into a path like
   "/path/to/ligo_list_helpers/list.mligo"

   To resolve the external packages used by ligo-list-helpers
   [get_dependencies] will give a list of paths of dependencies. *)

let get_dependencies ~file = function
  None -> []
| Some {resolutions; _} ->
    let path = Path.get_absolute_path file in
    let rec aux path =
      let predicate (Path mod_path, _) =
        Path.equal (Path.v mod_path) path in
      let resolution = List.find resolutions ~f:predicate
      in match resolution with
          Some (_, paths) -> paths
        | None when Path.is_root path -> []
        | None -> aux (Path.dirpath path)
    in
    aux path

(* The call [find_external_file ~file ~inclusion_paths] specifically
   resolves files for LIGO packages downloaded via esy.

   The [inclusion_paths] contains a list of paths of the form
   {package-name}__{version}__{hash}
   e.g. /path/to/ligo/cache/ligo_list_helpers__1.0.0__bf074147

   a ligo package will be used in #import or #include,
   e.g. #import "ligo-list-helpers/list.mligo" "ListHelpers"

   To correctly resolve the path for #import or #include we split the
   path into two parts: the package name and the rest of the path,
   e.g. "ligo-list-helpers/list.mligo" is split into
    package name = ligo-list-helpers
    rest of path = list.mligo

   Then we look for a path corresponding to package name in the
   [inclusion_paths]. *)

let find_external_file ~file ~inclusion_paths =
  let find_in_inclusion_paths ?scope pkg_name =
    let normalized_pkg_name = Esy.normalize_pkg_name ?scope pkg_name in
    let predicate (Path pkg_path) =
      String.(normalized_pkg_name = Esy.extract_pkg_name pkg_path)
    in List.find inclusion_paths ~f:predicate
  in
  let* segs = Path.segs (Path.v file) in
  let* segs =
    match segs with
      scope :: pkg_name :: rest_of_path
        when Core.String.is_prefix ~prefix:"@" scope ->
        (* scoped npm packages are of the form `@scope/pkg` *)
      let scope = Core.String.chop_prefix_exn ~prefix:"@" scope in
      let* Path dir = find_in_inclusion_paths ~scope pkg_name in
      let rest_of_path =
        String.concat ~sep:Filename.dir_sep rest_of_path
      in Some (Path.join dir rest_of_path)
    | [pkg_name] -> None
    | pkg_name :: rest_of_path ->
       let* Path dir = find_in_inclusion_paths pkg_name in
       let rest_of_path =
         String.concat ~sep:Filename.dir_sep rest_of_path
       in Some (Path.join dir rest_of_path)
    | _ -> None
  in Some segs

(* The function [pp] pretty-prints module resolutions. *)

let pp ppf (mod_res : t) =
  let {root_path; resolutions} = mod_res in
  let Path root_path = root_path in
  let () = Format.fprintf ppf "Root path   = %s\n" root_path in
  let () = Format.fprintf ppf "Resolutions =\n" in
  let path_fmt (Path p) = Format.fprintf ppf "\t%s\n" p in
  let pp_inclusion_paths ppf is = List.iter is ~f:path_fmt in
  let resolution_fmt (Path k, v) =
    Format.fprintf ppf "%s = [\n%a\n]\n" k pp_inclusion_paths v
  in List.iter resolutions ~f:resolution_fmt

(* Utilities *)

module Helpers =
  struct
    (* The function [resolve file_name module_resolutions] is a helper
       used by REPL & Test.originate_from_file to get the path of LIGO
       package e.g. In the REPL [#use
       'ligo-list-helpers/list.mligo';;] the pathe to #use will be
       resolved using this function, which internally calls the
       functions [get_root_dependencies] & [find_external_file] *)

    let resolve ~file module_resolutions =
      if Stdlib.Sys.file_exists file then file
      else
        let inclusion_paths =
          get_root_dependencies module_resolutions in
        let external_file  =
          find_external_file ~file ~inclusion_paths in
        match external_file with
          Some external_file -> external_file
        | None -> file
  end
