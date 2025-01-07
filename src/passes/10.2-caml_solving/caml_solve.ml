open Ocaml_common
open Ligo_prim
open Ast_core
open Caml_error

(* TODO: either there should be no errors here
  or we should support error recovery *)
(* TODO: this file handles name solving, reducing patterns and recursion *)
(* TODO: drop all Location.dummy *)
let fresh_value ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Value_var.fresh ~loc:Location.dummy ~name ~generated:false ()


let fresh_type ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Type_var.fresh ~loc:Location.dummy ~name ~generated:false ()


let fresh_module ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Module_var.fresh ~loc:Location.dummy ~name ~generated:false ()


module OCaml = struct
  module Env = Env
  module Types = Ocaml_common.Types
end

module Context : sig
  type context

  (* vars *)
  val enter_value : Ident.t -> Value_var.t -> context -> context
  val enter_value_external : Ident.t -> context -> context
  val enter_type : Ident.t -> Type_var.t -> context -> context
  val enter_type_predef_unsupported : Ident.t -> context -> context

  val enter_module
    :  Ident.t
    -> Module_var.t
    -> context
    -> (context -> context * 'k)
    -> context * 'k

  val enter_signature
    :  Ident.t
    -> Module_var.t
    -> context
    -> (context -> context * 'k)
    -> context * 'k

  val solve_value_path
    :  loc:Location.t
    -> Path.t
    -> context
    -> Value_var.t Module_access.t

  val solve_type_path : loc:Location.t -> Path.t -> context -> Type_var.t Module_access.t

  val solve_module_path
    :  loc:Location.t
    -> Path.t
    -> context
    -> Module_var.t Module_access.t

  (* external *)
  val run : (context -> 'k) -> 'k
end = struct
  (* TODO: core map *)
  module String_map = Stdlib.Map.Make (String)

  type value_info =
    | Value_external
    | Value_subst_to of Value_var.t

  type type_info =
    | Type_unsupported_predef
    | Type_subst_to of Type_var.t

  type md_context =
    { md_values : value_info String_map.t
    ; md_types : type_info String_map.t
    ; md_modules : (Module_var.t * md_context) String_map.t
    ; md_signatures : (Module_var.t * md_context) String_map.t
    }

  (* TODO: try with in all OCaml functions *)
  type context =
    { values : value_info Ident.Map.t
    ; types : type_info Ident.Map.t
    ; modules : (Module_var.t * md_context) Ident.Map.t
    ; signatures : (Module_var.t * md_context) Ident.Map.t
    ; local : md_context
    }

  let empty_local =
    { md_values = String_map.empty
    ; md_types = String_map.empty
    ; md_modules = String_map.empty
    ; md_signatures = String_map.empty
    }


  let empty =
    { values = Ident.Map.empty
    ; types = Ident.Map.empty
    ; modules = Ident.Map.empty
    ; signatures = Ident.Map.empty
    ; local = empty_local
    }


  let run k = k empty

  let enter_value_info ident value_info ctx =
    let name = Ident.name ident in
    let values = Ident.Map.add ident value_info ctx.values in
    let md_values = String_map.add name value_info ctx.local.md_values in
    { ctx with values; local = { ctx.local with md_values } }


  let enter_value ident value_var ctx =
    enter_value_info ident (Value_subst_to value_var) ctx


  let enter_value_external ident ctx = enter_value_info ident Value_external ctx

  let enter_type_info ident type_info ctx =
    let { values = _; types; modules = _; signatures = _; local } = ctx in
    let name = Ident.name ident in
    let types = Ident.Map.add ident type_info types in
    let md_types = String_map.add name type_info ctx.local.md_types in
    { ctx with types; local = { ctx.local with md_types } }


  let enter_module ident module_var ctx k =
    let { values = _; types = _; modules; signatures = _; local } = ctx in
    let inner_ctx, x = k { ctx with local = empty_local } in
    let name = Ident.name ident in
    let mod_data = module_var, inner_ctx.local in
    let modules = Ident.Map.add ident mod_data modules in
    let local =
      let { md_values = _; md_types = _; md_modules; md_signatures = _ } = local in
      let md_modules = String_map.add name mod_data md_modules in
      { local with md_modules }
    in
    { ctx with modules; local }, x


  let enter_signature ident module_var ctx k =
    let { values = _; types = _; modules = _; signatures; local } = ctx in
    let inner_ctx, x = k { ctx with local = empty_local } in
    let name = Ident.name ident in
    let mod_data = module_var, inner_ctx.local in
    let signatures = Ident.Map.add ident mod_data signatures in
    let local =
      let { md_values = _; md_types = _; md_modules = _; md_signatures } = local in
      let md_signatures = String_map.add name mod_data md_signatures in
      { local with md_signatures }
    in
    { ctx with signatures; local }, x


  let enter_type ident type_var ctx =
    let type_info = Type_subst_to type_var in
    enter_type_info ident type_info ctx


  let enter_type_predef_unsupported ident ctx =
    enter_type_info ident Type_unsupported_predef ctx


  let error_unexpected_module ~loc () =
    raise_error @@ { err_loc = loc; err_tag = E_solve_error_unexpected_module }


  let error_unexpected_value ~loc () =
    raise_error @@ { err_loc = loc; err_tag = E_solve_error_unexpected_value }


  let error_unexpected_type ~loc () =
    raise_error @@ { err_loc = loc; err_tag = E_solve_error_unexpected_type }


  let error_functor_not_supported ~loc () =
    raise_error @@ { err_loc = loc; err_tag = E_solve_error_functor_not_supported }


  (* TODO: drop all failwith *)
  (* TODO: reduce boilerplate below *)

  let rec solve_module_path ~loc path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (* TODO: use list not empty here *)
      (match Ident.Map.find_opt ident ctx.modules with
      | Some (module_, ctx) -> (module_, []), ctx
      | None -> error_unexpected_module ~loc ())
    | Pdot (left, right) ->
      let (rev_left_hd, rev_left_tl), ctx = solve_module_path ~loc left ctx in
      (match String_map.find_opt right ctx.md_modules with
      | Some (module_, ctx) -> (module_, rev_left_hd :: rev_left_tl), ctx
      | None -> error_unexpected_module ~loc ())
    | Papply (_, _) -> error_functor_not_supported ~loc ()


  let solve_value_path ~loc path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident ctx.values with
      | Some Value_external -> failwith "external value reached"
      | Some (Value_subst_to value) ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = value }
      | None -> error_unexpected_value ~loc ())
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), ctx = solve_module_path ~loc module_ ctx in
      (match String_map.find_opt right ctx.md_values with
      | Some Value_external -> failwith "external value reached"
      | Some (Value_subst_to value) ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = value }
      | None -> error_unexpected_value ~loc ())
    | Papply (_, _) -> error_functor_not_supported ~loc ()


  let solve_type_path ~loc path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident ctx.types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = type_ }
      | None ->
        Format.eprintf "ident: %a@.%!" Ident.print ident;
        error_unexpected_type ~loc ())
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), ctx = solve_module_path ~loc module_ ctx in
      (match String_map.find_opt right ctx.md_types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = type_ }
      | None -> error_unexpected_type ~loc ())
    | Papply (_, _) -> error_functor_not_supported ~loc ()


  let solve_module_path ~loc path ctx =
    let (rev_module_hd, rev_module_tl), ctx = solve_module_path ~loc path ctx in
    let module_path = List.rev rev_module_tl in
    Module_access.{ module_path; element = rev_module_hd }
end

open Context
open Caml_core
open Caml_pre_core

(* TODO: merge ctx and vars *)
let rec solve_type ctx vars typ_ =
  let Caml_core.{ type_desc; type_loc = loc } = typ_ in
  match type_desc with
  | T_var (name, id) ->
    let var =
      match Hashtbl.find vars id with
      | Some var -> var
      | None ->
        (* TODO: name *)
        let var = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
        Hashtbl.set vars ~key:id ~data:var;
        var
    in
    type_wrap loc @@ T_var var
  | T_constr (path, args) ->
    let path = solve_type_path ~loc path ctx in
    let args = List.map args ~f:(solve_type ctx vars) in
    type_wrap loc @@ T_constr (path, args)
  | T_arrow (param, return) ->
    let param = solve_type ctx vars param in
    let return = solve_type ctx vars return in
    (* TODO: param names? *)
    type_wrap loc @@ T_arrow (param, return)
  | T_tuple fields ->
    let fields = List.map fields ~f:(fun field -> solve_type ctx vars field) in
    type_wrap loc @@ T_tuple fields
  | T_forall (bound, body) ->
    (* TODO: this should fail if not poly *)
    (* TODO: test poly and univar *)
    solve_type_forall loc ctx vars ~bound body
  | T_error error -> type_wrap loc @@ T_error error


and solve_type_forall loc ctx vars ~bound body =
  match bound with
  | [] -> solve_type ctx vars body
  | (name, id) :: bound ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_forall loc ctx vars ~bound body in
    Hashtbl.remove vars id;
    type_wrap loc @@ T_forall (var, body)


let solve_type_poly ctx vars type_ =
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
  let Caml_core.{ type_desc = _; type_loc = loc } = type_ in
  let type_ = solve_type ctx vars type_ in
  let internal_vars =
    List.filter_map (Hashtbl.to_alist vars) ~f:(fun (id, var) ->
        match Hashtbl.mem external_vars id with
        | true -> None
        | false ->
          Hashtbl.remove vars id;
          Some var)
  in
  List.fold_left internal_vars ~init:type_ ~f:(fun body var ->
      type_wrap loc @@ T_forall (var, body))


let rec solve_type_decl ctx vars decl =
  let Caml_core.{ type_decl_desc; type_decl_params; type_decl_loc = loc } = decl in
  let params, body =
    solve_type_decl_lambda ctx vars loc type_decl_params type_decl_desc
  in
  List.iter type_decl_params ~f:(fun (_name, id) -> Hashtbl.remove vars id);
  type_decl_wrap loc params body


and solve_type_decl_record_fields ctx vars fields =
  let fields =
    List.map fields ~f:(fun decl_label ->
        let { dl_id; dl_type; dl_loc } = decl_label in
        let label = Ident.name dl_id in
        let type_ = solve_type_poly ctx vars dl_type in
        Label.Label (label, dl_loc), type_)
  in
  (* TODO: this exn? Check all of them *)
  Label.Map.of_alist_exn fields


and solve_type_decl_desc ctx vars desc =
  (* TODO: this is really weird right now *)
  match (desc : Caml_core.type_decl_desc) with
  | T_record fields ->
    let fields = solve_type_decl_record_fields ctx vars fields in
    T_record fields
  | T_variant cases ->
    let cases =
      List.map cases ~f:(fun decl_case ->
          match decl_case with
          | C_tuple { dc_id; dc_fields; dc_loc } ->
            let dc_id =
              (* TODO: disgusting hack *)
              match Ident.name dc_id with
              | "true" -> Label.Label ("True", dc_loc)
              | "false" -> Label.Label ("False", dc_loc)
              | name -> Label.Label (name, dc_loc)
            in
            let dc_fields =
              List.map dc_fields ~f:(fun field -> solve_type ctx vars field)
            in
            (* TODO: helpers here? *)
            dc_id, C_tuple { dc_id; dc_fields; dc_loc }
          | C_record { dc_id; dc_fields; dc_loc } ->
            let dc_id = Label.Label (Ident.name dc_id, dc_loc) in
            let dc_fields = solve_type_decl_record_fields ctx vars dc_fields in
            dc_id, C_record { dc_id; dc_fields; dc_loc })
    in
    let cases = Label.Map.of_alist_exn cases in
    T_variant cases
  | T_alias manifest ->
    (* TODO: not poly tho *)
    let manifest = solve_type_poly ctx vars manifest in
    T_alias manifest
  | T_error error -> T_error error


and solve_type_decl_lambda ctx vars loc params desc =
  match params with
  | [] -> [], solve_type_decl_desc ctx vars desc
  | (name, id) :: params ->
    (* TODO: fresh something *)
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let params, body = solve_type_decl_lambda ctx vars loc params desc in
    Hashtbl.remove vars id;
    var :: params, body


let rec solve_pat ctx vars pat =
  let Caml_core.{ pat_desc; pat_type; pat_loc = loc } = pat in
  (* TODO: ascription in all types *)
  (* TODO: dummy *)
  (* TODO: poly type *)
  let type_ = solve_type ctx vars pat_type in
  match pat_desc with
  | P_unit -> ctx, pat_wrap loc type_ @@ P_unit
  | P_var ident ->
    let var = fresh_value ident in
    let ctx = enter_value ident var ctx in
    ctx, pat_wrap loc type_ @@ P_var var
  | P_tuple fields ->
    let ctx, rev_fields =
      List.fold_left fields ~init:(ctx, []) ~f:(fun (ctx, rev_fields) pat ->
          let ctx, field = solve_pat ctx vars pat in
          ctx, field :: rev_fields)
    in
    let fields = List.rev rev_fields in
    ctx, pat_wrap loc type_ @@ P_tuple fields
  | P_record fields ->
    let ctx, rev_fields =
      List.fold_left fields ~init:(ctx, []) ~f:(fun (ctx, rev_fields) (label, pat) ->
          let ctx, pat = solve_pat ctx vars pat in
          ctx, (label, pat) :: rev_fields)
    in
    let fields = List.rev rev_fields in
    ctx, pat_wrap loc type_ @@ P_record (Record.of_list fields)
  | P_variant (label, fields) ->
    (match label with
    (* TODO: this is very hackish *)
    | Label ("()", _loc) ->
      (* TODO: assert fields *)
      (* assert (List.is_empty fields); *)
      ctx, pat_wrap loc type_ @@ P_unit
    (* | Label ("::", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      ctx, pat_wrap loc @@ E_constant { cons_name = C_CONS; arguments }
    | Label ("[]", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      ctx, pat_wrap loc @@ E_constant { cons_name = C_LIST_EMPTY; arguments } *)
    | label ->
      let ctx, fields = solve_pat ctx vars fields in
      ctx, pat_wrap loc type_ @@ P_variant (label, fields))
  | P_error error ->
    (* TODO: why raise here *)
    ctx, pat_wrap loc type_ @@ P_error error


let solve_var_pat ctx vars pat =
  let Caml_core.{ pat_desc; pat_type; pat_loc = loc } = pat in
  let type_ = solve_type ctx vars pat_type in
  match pat_desc with
  | P_var ident ->
    let var = fresh_value ident in
    let ctx = enter_value ident var ctx in
    ctx, var_pat_wrap loc type_ @@ VP_var var
  | P_unit | P_tuple _ | P_record _ | P_variant _ ->
    (* TODO: making this here is weird *)
    ( ctx
    , var_pat_wrap loc type_
      @@ VP_error { err_tag = E_only_variable_patterns_supported; err_loc = loc } )
  | P_error error -> ctx, var_pat_wrap loc type_ @@ VP_error error


let rec solve_expr ctx vars expr =
  let Caml_core.{ expr_desc; expr_type; expr_loc = loc } = expr in
  (* TODO: use this expr_type *)
  let type_ = solve_type ctx vars expr_type in
  match expr_desc with
  | E_var path ->
    let var = solve_value_path ~loc path ctx in
    expr_wrap loc type_ @@ E_var var
  | E_literal lit -> expr_wrap loc type_ @@ E_literal lit
  | E_let (binder, attr, value, body) ->
    (* TODO: recursive *)
    let inner_ctx, binder = solve_pat ctx vars binder in
    let foralls, value = solve_expr_poly ctx vars value in
    let body = solve_expr inner_ctx vars body in
    expr_wrap loc type_ @@ E_let { binder; foralls; attr; value; body }
  | E_let_module (ident, mod_expr, body) ->
    let var = fresh_module ident in
    let inner_ctx, mod_expr =
      enter_module ident var ctx @@ fun ctx -> solve_mod_expr ctx mod_expr
    in
    let body = solve_expr inner_ctx vars body in
    expr_wrap loc type_ @@ E_let_module (var, mod_expr, body)
  | E_lambda (param, body) ->
    let inner_ctx, param = solve_var_pat ctx vars param in
    let body = solve_expr inner_ctx vars body in
    expr_wrap loc type_ @@ E_lambda (param, body)
  | E_lambda_rec { self; param; body } ->
    let inner_ctx, self = solve_var_pat ctx vars self in
    let inner_ctx, param = solve_var_pat inner_ctx vars param in
    (* TODO: output_type *)
    (* TODO: forcelambdarec *)
    let body = solve_expr inner_ctx vars body in
    expr_wrap loc type_ @@ E_lambda_rec { self; param; body }
  | E_apply (lambda, args) ->
    let lambda = solve_expr ctx vars lambda in
    let args = List.map args ~f:(fun arg -> solve_expr ctx vars arg) in
    expr_wrap loc type_ @@ E_apply (lambda, args)
  | E_match (matchee, cases) ->
    let matchee = solve_expr ctx vars matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let ctx, pat = solve_pat ctx vars pat in
          let body = solve_expr ctx vars body in
          pat, body)
    in
    expr_wrap loc type_ @@ E_match (matchee, cases)
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:(fun field -> solve_expr ctx vars field) in
    expr_wrap loc type_ @@ E_tuple fields
  | E_constructor (constructor, fields) ->
    (* TODO: high priority *)
    (match constructor with
    (* TODO: this is very hackish *)
    | Label ("()", _loc) ->
      assert (List.is_empty fields);
      expr_wrap loc type_ @@ E_literal Literal_unit
    | Label ("::", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      expr_wrap loc type_ @@ E_constant { cons_name = C_CONS; arguments }
    | Label ("[]", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      expr_wrap loc type_ @@ E_constant { cons_name = C_LIST_EMPTY; arguments }
    | _ ->
      (* TODO: location? *)
      let constructor =
        let open Label in
        (* TODO: this is clearly hackish *)
        match constructor with
        | Label ("true", loc) -> Label ("True", loc)
        | Label ("false", loc) -> Label ("False", loc)
        | Label (_label, _loc) -> constructor
      in
      let fields = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      expr_wrap loc type_ @@ E_constructor (constructor, fields))
  | E_record fields ->
    let fields =
      List.map fields ~f:(fun (label, field) -> label, solve_expr ctx vars field)
    in
    expr_wrap loc type_ @@ E_record (Record.of_list fields)
  | E_field (struct_, label) ->
    let struct_ = solve_expr ctx vars struct_ in
    expr_wrap loc type_ @@ E_field (struct_, label)
  | E_error error -> expr_wrap loc type_ @@ E_error error


and solve_expr_poly ctx vars expr =
  (* TODO: this is also duplicated *)
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
  (* TODO: looks weird to extract here *)
  let expr = solve_expr ctx vars expr in
  let foralls =
    List.filter_map (Hashtbl.to_alist vars) ~f:(fun (id, var) ->
        match Hashtbl.mem external_vars id with
        | true -> None
        | false ->
          Hashtbl.remove vars id;
          Some var)
  in
  foralls, expr


and solve_module ctx module_ =
  let ctx, rev_decl =
    List.fold_left module_ ~init:(ctx, []) ~f:(fun (ctx, rev_module) decl ->
        let ctx, decl = solve_decl ctx decl in
        (* TODO: this is ugly *)
        match decl with
        | None -> ctx, rev_module
        | Some decl -> ctx, decl :: rev_module)
  in
  ctx, List.rev rev_decl


and solve_decl ctx decl =
  let vars = Hashtbl.create (module Int) in
  let ctx, decl = solve_decl_inner ctx vars decl in
  assert (Hashtbl.is_empty vars);
  ctx, decl


and solve_decl_inner ctx vars decl =
  let Caml_core.{ decl_desc; decl_loc = loc } = decl in
  match decl_desc with
  | D_let (binder, attr, value) ->
    let inner_ctx, binder = solve_var_pat ctx vars binder in
    let foralls, value = solve_expr_poly ctx vars value in
    inner_ctx, Some (decl_wrap loc @@ D_let { binder; foralls; value; attr })
  | D_type (ident, type_decl) ->
    let type_decl = solve_type_decl ctx vars type_decl in
    let var = fresh_type ident in
    let ctx = Context.enter_type ident var ctx in
    (* TODO: attributes here *)
    let attr = Type_or_module_attr.default_attributes in
    ctx, Some (decl_wrap loc @@ D_type (var, attr, type_decl))
  | D_external ident ->
    (* TODO: remove the need for this? *)
    let ctx = enter_value_external ident ctx in
    ctx, None
  | D_type_predef (ident, literal, arity) ->
    (* TODO: this is brittle, what if duplicated? *)
    let var = Type_var.of_input_var ~loc @@ Literal_types.to_string @@ literal in
    let ctx = enter_type ident var ctx in
    ctx, Some (decl_wrap loc @@ D_type_predef (var, literal, arity))
  | D_type_unsupported ident ->
    let ctx = enter_type_predef_unsupported ident ctx in
    ctx, None
  | D_module (ident, attr, mod_expr) ->
    let var = fresh_module ident in
    let ctx, module_ =
      enter_module ident var ctx @@ fun ctx -> solve_mod_expr ctx mod_expr
    in
    ctx, Some (decl_wrap loc @@ D_module (var, attr, module_))
  | D_module_include mod_expr ->
    let ctx, module_ = solve_mod_expr ctx mod_expr in
    ctx, Some (decl_wrap loc @@ D_module_include module_)
  | D_module_type (ident, attr, sig_expr) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_signature ident var ctx @@ fun ctx -> solve_sig_expr ctx sig_expr
    in
    ctx, Some (decl_wrap loc @@ D_module_type (var, attr, signature))
  | D_error error -> ctx, Some (decl_wrap loc @@ D_error error)


and solve_mod_expr ctx mod_expr =
  let Caml_core.{ mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_var var ->
    (* TODO: why the unchanged context is returned here? *)
    let var = solve_module_path ~loc var ctx in
    ctx, mod_expr_wrap loc @@ M_var var
  | M_struct decls ->
    let ctx, decls = solve_module ctx decls in
    ctx, mod_expr_wrap loc @@ M_struct decls


and solve_signature ctx sig_ =
  let ctx, rev_sig =
    List.fold_left sig_ ~init:(ctx, []) ~f:(fun (ctx, rev_sig) decl ->
        let ctx, sigi = solve_sigi ctx decl in
        ctx, sigi :: rev_sig)
  in
  ctx, List.rev rev_sig


and solve_sigi ctx sigi =
  (* TODO: duplicated from solve_decl *)
  let vars = Hashtbl.create (module Int) in
  let ctx, sigi = solve_sigi_inner ctx vars sigi in
  assert (Hashtbl.is_empty vars);
  ctx, sigi


and solve_sigi_inner ctx vars sigi =
  let Caml_core.{ sig_item_desc; sig_item_loc = loc } = sigi in
  match sig_item_desc with
  | S_value (ident, attr, type_) ->
    let var = fresh_value ident in
    let type_ = solve_type_poly ctx vars type_ in
    let ctx = enter_value ident var ctx in
    ctx, sig_item_wrap loc @@ S_value (var, attr, type_)
  | S_type (ident, type_decl) ->
    let type_decl = solve_type_decl ctx vars type_decl in
    let var = fresh_type ident in
    let ctx = enter_type ident var ctx in
    let attr = SigTypeAttr.default_attributes in
    ctx, sig_item_wrap loc @@ S_type (var, attr, type_decl)
  | S_module (ident, mod_sig) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_module ident var ctx @@ fun ctx -> solve_signature ctx mod_sig
    in
    (* TODO: use this inner_ctx? *)
    ctx, sig_item_wrap loc @@ S_module (var, signature)
  | S_module_type (ident, signature) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_module ident var ctx @@ fun ctx -> solve_signature ctx signature
    in
    ctx, sig_item_wrap loc @@ S_module_type (var, signature)
  | S_error error -> ctx, sig_item_wrap loc @@ S_error error


and solve_sig_expr ctx sig_expr =
  let Caml_core.{ sig_expr_desc; sig_expr_loc = loc } = sig_expr in
  match sig_expr_desc with
  | S_var var ->
    let var = solve_module_path ~loc var ctx in
    ctx, sig_expr_wrap loc @@ S_var var
  | S_sig signature ->
    let ctx, signature = solve_signature ctx signature in
    ctx, sig_expr_wrap loc @@ S_sig signature


let solve_module module_ =
  (* TODO: proper location here *)
  let loc = Location.dummy in
  Caml_error.wrap_exn ~loc (fun () ->
      let _ctx, module_ = Context.run @@ fun ctx -> solve_module ctx module_ in
      module_)
