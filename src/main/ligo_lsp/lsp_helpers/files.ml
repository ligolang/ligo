let list_directory ?(include_library = false) (dir : Path.t) : Path.t list =
  Ligo_api.Api_helpers.list_directory ~include_library (Path.to_string dir)
  |> List.map ~f:Path.from_absolute


let list_library_files (dir : Path.t) (mod_res : Preprocessor.ModRes.t option)
    : string list
  =
  Option.value
    ~default:[]
    (let open Option.Let_syntax in
    let%bind installation_json_path =
      Preprocessor.ModRes.Esy.installation_json_path (Path.to_string dir)
    in
    let%bind installation_json =
      try Some (Yojson.Safe.from_file installation_json_path) with
      | _ -> None
    in
    let module SMap = Caml.Map.Make (String) in
    let%bind assoc_list =
      match installation_json with
      | `Assoc lst -> Some lst
      | _ -> None
    in
    let path_to_package_map : string SMap.t =
      List.fold_left assoc_list ~init:SMap.empty ~f:(fun mp (k, v) ->
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
               return @@ SMap.add path_to_sources package_name mp)
          | _ -> mp)
    in
    let root_deps = Preprocessor.ModRes.get_root_dependencies mod_res in
    let open Simple_utils.Function in
    return
    @@ List.concat
    @@ List.filter_map root_deps ~f:(fun (Path path) ->
           let%bind package_name = SMap.find_opt path path_to_package_map in
           let path = Path.from_absolute path in
           list_directory ~include_library:true path
           |> List.map ~f:(Filename.concat package_name <@ Path.make_relative path)
           |> return))


(** {!Path.from_absolute} is a very expensive function, so we may make a table to hold the
    known normalized paths as an optimization. Use this instead of calling that function
    in a loop. Warning: the contents of the normalization are cached, meaning that they
    won't account for changes in the disk. It is recommended to not persist this table for
    long operations. *)
let with_normalized_files (type cont) : f:(normalize:(string -> Path.t) -> cont) -> cont =
 fun ~f ->
  let file_normalization_tbl = Hashtbl.create (module String) in
  f ~normalize:(fun file ->
      Hashtbl.find_or_add file_normalization_tbl file ~default:(fun () ->
          Path.from_absolute file))


(** Get the normalization table created by !{with_normalized_files}. Warning: the contents
    of the normalization are cached, meaning that they won't account for changes in the
    disk. It is recommended to not persist this table for long operations. *)
let create_normalization : string -> Path.t =
  with_normalized_files ~f:(fun ~normalize -> normalize)
