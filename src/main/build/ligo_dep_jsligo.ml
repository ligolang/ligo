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
