open Core
open Simple_utils
module Deps = Set.Make (Filename)
module Locations = Map.Make (Filename)

let dependencies prg =
  let rec f decl (set, locations) =
    let Location.{ wrap_content = decl; location = loc } = decl in
    match decl with
    | Ast_core.D_module { module_; _ } ->
      (match Location.unwrap module_ with
      | M_struct decls -> List.fold_right ~init:(set, locations) decls ~f
      | _ -> set, locations)
    | D_import import_decl ->
      (match import_decl with
      | Import_rename _ -> set, locations
      | Import_all_as { module_str; _ } ->
        Set.add set module_str, Map.set locations ~key:module_str ~data:loc
      | Import_selected { module_str; _ } ->
        Set.add set module_str, Map.set locations ~key:module_str ~data:loc)
    | _ -> set, locations
  in
  let set, locations = List.fold_right ~init:(Deps.empty, Locations.empty) prg ~f in
  set
  |> Set.to_list
  |> List.map ~f:(fun module_str ->
         Location.wrap ~loc:(Map.find_exn locations module_str) module_str)


let imports_of_deps ~options file_name deps =
  let dirname = Filename.dirname file_name in
  let dirnames = dirname :: options.Compiler_options.frontend.libraries in
  let orig_file_name = file_name in
  List.filter_map deps ~f:(fun dep ->
      List.find_map dirnames ~f:(fun dirname ->
          let import_str = Location.unwrap dep in
          let file_name =
            match Filename.split_extension import_str with
            | name, None -> name ^ ".jsligo"
            | _ -> import_str
          in
          let file_name = Filename.concat dirname file_name in
          let%bind.Option file_name =
            if Filename.equal orig_file_name file_name then None else Some file_name
          in
          let%bind.Option stat =
            try Some (Core_unix.stat file_name) with
            | _ -> None
          in
          let%map.Option file_name =
            match stat.st_kind with
            | S_REG -> Some file_name
            | _ -> None
          in
          let file_name = Helpers.normalize_path file_name in
          let module_name = file_name in
          let location = dep.location in
          ( BuildSystem.
              { code_input = Source_input.From_file file_name; module_name; location }
          , [ module_name ] )))
