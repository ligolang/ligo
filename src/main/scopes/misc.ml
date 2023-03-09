open Ligo_prim
open Types
module Bindings_map = Simple_utils.Map.Make (Value_var)
module Pattern = Ast_typed.Pattern

type bindings_map = Ast_typed.type_expression Bindings_map.t

let rec extract_variable_types
    : bindings_map -> Ast_typed.declaration_content -> bindings_map
  =
 fun prev decl ->
  let add env b =
    let aux
        :  Ast_typed.expression_variable * Ast_typed.type_expression
        -> Ast_typed.expression_variable * Ast_typed.type_expression
      =
     fun (v, t) ->
      let t' =
        match t.orig_var with
        | Some t' -> { t with type_content = T_variable t' }
        | None -> t
      in
      v, t'
    in
    let b' = List.map ~f:aux b in
    Bindings_map.add_bindings b' env
  in
  let aux : bindings_map -> Ast_typed.expression -> bindings_map =
   fun env exp ->
    let return = add env in
    let loc = exp.location in
    match exp.expression_content with
    | E_literal _
    | E_application _
    | E_raw_code _
    | E_constructor _
    | E_assign _
    | E_deref _
    | E_while _
    | E_type_abstraction _
    | E_mod_in _
    | E_record _
    | E_accessor _
    | E_update _
    | E_constant _ -> return []
    | E_type_inst _ -> return [] (* TODO *)
    | E_variable v -> return [ v, exp.type_expression ]
    (* Wait what??? *)
    | E_lambda { binder; _ } ->
      let rec in_t t =
        match t.Ast_typed.type_content with
        | T_arrow { type1; _ } -> type1
        | T_for_all { type_; _ } -> in_t type_
        | _ -> failwith "lambda does not have type arrow"
      in
      let in_t = in_t exp.type_expression in
      return [ Param.get_var binder, in_t ]
    | E_recursive { fun_name; fun_type; lambda = { binder; _ }; force_lambdarec = _ } ->
      let in_t =
        match fun_type.type_content with
        | T_arrow { type1; _ } -> type1
        | _ -> failwith "rec fun does not have type arrow"
      in
      return [ fun_name, fun_type; Param.get_var binder, in_t ]
    | E_let_mut_in { let_binder; rhs = _; _ } | E_let_in { let_binder; rhs = _; _ } ->
      return
      @@ List.map
           ~f:(fun binder -> Binder.get_var binder, Binder.get_ascr binder)
           (Pattern.binders let_binder)
    | E_matching { matchee = _; cases } ->
      let bindings =
        List.concat
        @@ List.map cases ~f:(fun { pattern; _ } ->
               let binders = Pattern.binders pattern in
               List.map binders ~f:(fun b -> Binder.get_var b, Binder.get_ascr b))
      in
      return bindings
    | E_module_accessor { element = e; _ } -> return [ e, exp.type_expression ]
    | E_for { binder; start; _ } -> return [ binder, start.type_expression ]
    | E_for_each { fe_binder = binder1, Some binder2; collection; _ } ->
      let key_type, val_type = Ast_typed.get_t_map_exn collection.type_expression in
      return [ binder1, key_type; binder2, val_type ]
    | E_for_each { fe_binder = binder, None; collection; _ } ->
      let type_ = collection.type_expression in
      if Ast_typed.is_t_set type_
      then return [ binder, Ast_typed.get_t_set_exn type_ ]
      else if Ast_typed.is_t_list type_
      then return [ binder, Ast_typed.get_t_list_exn type_ ]
      else if Ast_typed.is_t_map type_
      then (
        let k, v = Ast_typed.get_t_map_exn type_ in
        return [ binder, Ast_typed.t_pair ~loc k v ])
      else failwith "E_for_each type with 1 binder should have map, set or list type"
  in
  match decl with
  | D_value { attr = { hidden = true; _ }; _ } -> prev
  | D_irrefutable_match { attr = { hidden = true; _ }; _ } -> prev
  | D_value { binder; expr; _ } ->
    let prev = add prev [ Binder.get_var binder, expr.type_expression ] in
    Self_ast_typed.Helpers.fold_expression aux prev expr
  | D_irrefutable_match { pattern; expr; _ } ->
    let prev =
      let f acc binder = add acc [ Binder.get_var binder, expr.type_expression ] in
      List.fold (Pattern.binders pattern) ~f ~init:prev
    in
    Self_ast_typed.Helpers.fold_expression aux prev expr
  | D_type _ -> prev
  | D_module { module_; _ } ->
    (match module_.wrap_content with
    | M_variable _ -> prev
    | M_module_path _ -> prev
    | M_struct ds ->
      List.fold_left ds ~init:prev ~f:(fun prev d ->
          extract_variable_types prev d.wrap_content))


let resolve_if : with_types:bool -> bindings_map -> Value_var.t -> type_case =
 fun ~with_types bindings var ->
  if with_types
  then (
    let t_opt = Bindings_map.find_opt var bindings in
    match t_opt with
    | Some t -> Resolved t
    | None -> Unresolved)
  else Unresolved


let make_v_def
    :  with_types:bool -> ?core_type:Ast_core.type_expression -> bindings_map -> def_type
    -> Value_var.t -> Location.t -> Location.t -> def
  =
 fun ~with_types ?core_type bindings def_type var range body_range ->
  let type_case =
    match core_type with
    | Some t -> Core t
    | None -> resolve_if ~with_types bindings var
  in
  make_v_def (get_binder_name var) type_case def_type range body_range

let get_location_of_module_path : Module_var.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (Module_var.get_location m))
