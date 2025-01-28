(* TODO: this could be generated, this is pretty much just visit *)
(* TODO: this just happens because there is no errors in Ast_core *)

open Ligo_prim
open Caml_pre_core

let write_error errors ~loc error = errors := (error, loc) :: !errors

let rec collect_type errors typ_ =
  let { type_desc; type_loc = loc } = typ_ in
  match type_desc with
  | T_var _var -> ()
  | T_constr (path, args) -> List.iter args ~f:(fun type_ -> collect_type errors type_)
  | T_arrow (param, return) ->
    collect_type errors param;
    collect_type errors return
  | T_tuple fields -> List.iter fields ~f:(fun type_ -> collect_type errors type_)
  | T_forall (_var, body) -> collect_type errors body
  | T_error error -> write_error errors ~loc error


let rec collect_type_decl errors decl =
  let { type_decl_desc = body; type_decl_params = _; type_decl_loc = loc } = decl in
  match body with
  | T_record fields -> collect_type_record errors fields
  | T_variant cases ->
    Record.iter cases ~f:(fun decl_case ->
        match decl_case with
        | C_tuple { dc_id = _; dc_fields; dc_loc = _ } ->
          List.iter dc_fields ~f:(fun type_ -> collect_type errors type_)
        | C_record { dc_id = _; dc_fields; dc_loc = _ } ->
          collect_type_record errors dc_fields)
  | T_alias manifest ->
    (* TODO: not poly tho *)
    collect_type errors manifest
  | T_error error -> write_error errors ~loc error


and collect_type_record errors fields =
  Record.iter fields ~f:(fun type_ -> collect_type errors type_)


let rec collect_pat errors pat =
  let { pat_desc; pat_type; pat_loc = loc } = pat in
  match pat_desc with
  | P_unit -> ()
  | P_var var -> collect_type errors pat_type
  | P_tuple fields -> List.iter fields ~f:(fun pat -> collect_pat errors pat)
  | P_record fields -> Record.iter fields ~f:(fun pat -> collect_pat errors pat)
  | P_variant (_label, fields) -> collect_pat errors fields
  | P_error error -> write_error errors ~loc error


let collect_var_pat errors pat =
  let { var_pat_desc; var_pat_type; var_pat_loc = loc } = pat in
  match var_pat_desc with
  | VP_var var -> collect_type errors var_pat_type
  | VP_error error -> write_error errors ~loc error


let rec collect_expr errors expr =
  let { expr_desc; expr_type; expr_loc = loc } = expr in
  match expr_desc with
  | E_var _var -> ()
  | E_literal _lit -> ()
  | E_constant { cons_name; arguments } ->
    List.iter arguments ~f:(fun expr -> collect_expr errors expr)
  | E_let { binder; foralls = _; attr = _; value; body } ->
    collect_pat errors binder;
    collect_expr errors value;
    collect_expr errors body
  | E_let_module (_var, mod_expr, body) ->
    collect_mod_expr errors mod_expr;
    collect_expr errors body
  | E_lambda (param, body) ->
    collect_var_pat errors param;
    collect_expr errors body
  | E_lambda_rec { self; param; body } ->
    collect_var_pat errors self;
    collect_var_pat errors param;
    collect_expr errors body
  | E_apply (lambda, args) ->
    collect_expr errors lambda;
    List.iter args ~f:(fun arg -> collect_expr errors arg)
  | E_match (matchee, cases) ->
    collect_expr errors matchee;
    List.iter cases ~f:(fun (pat, body) ->
        collect_pat errors pat;
        collect_expr errors body)
  | E_tuple fields -> Nonempty_list.iter fields ~f:(fun expr -> collect_expr errors expr)
  | E_constructor (_constructor, fields) ->
    List.iter fields ~f:(fun expr -> collect_expr errors expr)
  | E_record fields -> Record.iter fields ~f:(fun expr -> collect_expr errors expr)
  | E_field (struct_, _label) -> collect_expr errors struct_
  | E_error error -> write_error errors ~loc error


and collect_module errors module_ =
  List.iter module_ ~f:(fun decl -> collect_decl errors decl)


and collect_decl errors decl =
  let { decl_desc; decl_loc = loc } = decl in
  match decl_desc with
  | D_let { binder; foralls = _; attr = _; value } ->
    collect_var_pat errors binder;
    collect_expr errors value
  | D_type (_var, _attr, type_decl) -> collect_type_decl errors type_decl
  | D_module (_var, _attr, mod_expr) -> collect_mod_expr errors mod_expr
  | D_module_include mod_expr -> collect_mod_expr errors mod_expr
  | D_module_type (_var, _attr, sig_expr) -> collect_sig_expr errors sig_expr
  | D_type_predef (_var, _literal, _arity) -> ()
  | D_type_unsupported -> ()
  | D_attribute -> ()
  | D_error error -> write_error errors ~loc error


and collect_mod_expr errors mod_expr =
  let { mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_var _path -> ()
  | M_struct decls -> collect_module errors decls


and collect_signature errors sig_ =
  List.iter sig_ ~f:(fun sigi -> collect_sigi errors sigi)


and collect_sigi errors sigi =
  let { sig_item_desc; sig_item_loc = loc } = sigi in
  match sig_item_desc with
  | S_value (_var, _attr, type_) -> collect_type errors type_
  | S_type (_var, _attr, type_decl) -> collect_type_decl errors type_decl
  | S_module (_var, mod_sig) -> collect_signature errors mod_sig
  | S_module_type (_var, signature) -> collect_signature errors signature
  | S_error error -> write_error errors ~loc error


and collect_sig_expr errors sig_expr =
  let { sig_expr_desc; sig_expr_loc = loc } = sig_expr in
  match sig_expr_desc with
  | S_var _path -> ()
  | S_sig signature -> collect_signature errors signature


let collect_module module_ =
  let errors = ref [] in
  collect_module errors module_;
  List.map !errors ~f:(fun (error, loc) ->
      let open Ast_core in
      match Location.is_dummy_or_generated loc with
      | true -> error
      | false ->
        let Caml_error.{ err_tag; err_loc } = error in
        (match Location.is_dummy_or_generated err_loc with
        | true -> { err_tag; err_loc }
        | false -> error))
