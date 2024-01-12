module AST = Ast_core
open Types

(** Given some signature:

    {[
      module type I = sig
        (* In no particular order: *)
        include sig ... end (* let's call it I1 *)
        include sig ... end (* I2 *)
        ...
        include I3
        include I4
        ...
      end
    ]}

    This function will inline every signature RECURSIVELY into [I] so it will look like
    this:

    {[
      module type I = sig
        (* contents of I1 and all its nested includes *)
        (* contents of I2 and all its nested includes *)
        include I3
        include I4
      end
    ]}

    Note that this function will NOT visit nested modules or module types. *)
let flatten_includes : Ast_core.signature -> Ast_core.signature =
 fun { items } ->
  let rec go acc =
    List.fold_right ~init:acc ~f:(fun item acc ->
        match Location.unwrap item with
        | Ast_core.S_include sig_expr ->
          (match Location.unwrap sig_expr with
          | S_sig { items } -> go acc items
          | S_path _mod_path ->
            let loc = Location.get_location item in
            Location.wrap ~loc (Ast_core.S_include sig_expr) :: acc)
        | S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _ ->
          item :: acc)
  in
  { items = go [] items }
