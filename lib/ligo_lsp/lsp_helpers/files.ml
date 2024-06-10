open Core
module ModRes = Preprocessor.ModRes
module Ligo_fun = Simple_utils.Ligo_fun

let ( <@ ) = Ligo_fun.( <@ )

(** Lists all files in a directory. *)

let list_directory
    ~(normalize : Path.normalization)
    ?(include_library = false)
    (dir : Path.t)
    : Path.t list
  =
  Ligo_api.Api_helpers.list_directory ~include_library (Path.to_string dir)
  |> List.map ~f:normalize

(** Lists all library files (i.e., [#import]ed files from the LIGO registry) in a
    directory. *)
let list_library_files
    ~(normalize : Path.normalization)
    (dir : Path.t)
    (mod_res : ModRes.t option)
    : string list
  =
  Option.value
    ~default:[]
    (let open Option.Let_syntax in
    let%bind installation_json_path =
      ModRes.Esy.installation_json_path (Path.to_string dir)
    in
    let%bind installation_json =
      try Some (Yojson.Safe.from_file installation_json_path) with
      | _ -> None
    in
    let%bind assoc_list =
      match installation_json with
      | `Assoc lst -> Some lst
      | _ -> None
    in
    let path_to_package_map : string String.Map.t =
      List.fold_left assoc_list ~init:String.Map.empty ~f:(fun mp (k, v) ->
          match v with
          | `String v ->
            Option.value
              ~default:mp
              (let%bind package_name =
                 match Package_management_alpha_shared.Name_version.of_string k with
                 | Ok name_version -> Some name_version.name
                 | Error _ -> None
               in
               let path_to_sources = FilePath.make_absolute (Path.to_string dir) v in
               return @@ Map.set mp ~key:path_to_sources ~data:package_name)
          | _ -> mp)
    in
    let root_deps = ModRes.get_root_dependencies mod_res in
    return
    @@ List.concat
    @@ List.filter_map root_deps ~f:(fun (Path path) ->
           let%bind package_name = Map.find path_to_package_map path in
           let path = normalize path in
           list_directory ~normalize ~include_library:true path
           |> List.map ~f:(Filename.concat package_name <@ Path.make_relative path)
           |> return))
