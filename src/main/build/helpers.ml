let internalize_typed (ds : Ast_typed.program) =
  let open Ast_typed in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | _ -> d in
  let f (d : _ Ast_typed.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds

let internalize_core (ds : Ast_core.module_) =
  let open Ast_core in
  let f (d : _) = match d with
    | Declaration_module { module_binder ; module_ ; module_attr = _ } ->
       let module_attr = { public = false ; hidden = true } in
       Declaration_module { module_binder ; module_ ; module_attr }
    | _ -> d in
  let f (d : _ Ast_core.location_wrap) = Simple_utils.Location.map f d in
  List.map ~f ds
