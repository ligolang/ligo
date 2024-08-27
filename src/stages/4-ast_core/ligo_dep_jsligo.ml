open Core
open Simple_utils

let dependencies prg =
  let rec f decl =
    match Location.unwrap decl with
    | Types.D_module { module_; _ } ->
      (match Location.unwrap module_ with
      | M_struct decls -> List.concat_map decls ~f
      | _ -> [])
    | D_import import_decl ->
      (match import_decl with
      | Import_rename _ -> []
      | Import_all_as { module_str; _ } -> [ module_str ]
      | Import_selected { module_str; _ } -> [ module_str ])
    | _ -> []
  in
  List.concat_map prg ~f
