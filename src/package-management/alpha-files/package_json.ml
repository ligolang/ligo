open Package_management_external_libs
module Semver = Ligo_semver
module DependencyMap = Caml.Map.Make (String)

type t =
  { dependencies : Semver.t DependencyMap.t
  ; dev_dependencies : Semver.t DependencyMap.t [@key "devDependencies"]
  }

let dependencies_y_to_map ~field_name = function
  | `Assoc kvs ->
    let init = Ok DependencyMap.empty in
    let f dependencies_map_acc (name, version_y) =
      match dependencies_map_acc, version_y with
      | Ok dependencies_map, `String version_str ->
        (match Semver.of_string version_str with
        | Some version -> Ok (DependencyMap.add name version dependencies_map)
        | None -> Error ("Could not parse version string: " ^ version_str ^ " for " ^ name))
      | Error e, _ -> Error e
      | _, y ->
        Error
          ("Unexpected value for version field. Expected string. Got "
          ^ Yojson.Safe.to_string y)
    in
    List.fold_left ~f ~init kvs
  | y ->
    Error
      (Printf.sprintf
         "Expected json for %s. Expected name-version key value pairs. Got %s"
         field_name
         (Yojson.Safe.to_string y))


let extract_dependencies =
  let init = None (* dependencies *), None (* dev_dependencies *) in
  let f acc kv_yojson =
    let dependencies, dev_dependencies = acc in
    match kv_yojson with
    | "dependencies", y ->
      Some (dependencies_y_to_map ~field_name:"dependencies" y), dev_dependencies
    | "dev_dependencies", y ->
      dependencies, Some (dependencies_y_to_map ~field_name:"dev_dependencies" y)
    | _, _ -> acc
  in
  List.fold_left ~f ~init


let to_yojson { dependencies; dev_dependencies } =
  let dependencies_to_y dependencies =
    let f (name, version) = name, `String (Semver.to_string version) in
    `Assoc (dependencies |> DependencyMap.bindings |> List.map ~f)
  in
  let dependencies_y = dependencies_to_y dependencies in
  let dev_dependencies_y = dependencies_to_y dev_dependencies in
  `Assoc [ "dependencies", dependencies_y; "dev_dependencies", dev_dependencies_y ]


let of_yojson = function
  | `Assoc kvs_y ->
    (match extract_dependencies kvs_y with
    | Some (Ok dependencies), Some (Ok dev_dependencies) ->
      Ok { dependencies; dev_dependencies }
    | None, Some (Ok dev_dependencies) ->
      let dependencies = DependencyMap.empty in
      Ok { dependencies; dev_dependencies }
    | Some (Ok dependencies), None ->
      let dev_dependencies = DependencyMap.empty in
      Ok { dependencies; dev_dependencies }
    | None, None ->
      let dev_dependencies = DependencyMap.empty in
      let dependencies = DependencyMap.empty in
      Ok { dependencies; dev_dependencies }
    | Some (Error e), _ -> Error e
    | _, Some (Error e) -> Error e)
  | y ->
    Error
      ("Unexpected package json structure. Expected a object. Got: "
      ^ Yojson.Safe.to_string y)
