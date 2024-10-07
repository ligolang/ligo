open Ocaml_common
open Ligo_prim
open Ast_core
open Caml_core

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
  type t = context

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

  val solve_value_path : Path.t -> context -> Value_var.t Module_access.t
  val solve_type_path : Path.t -> context -> Type_var.t Module_access.t
  val solve_module_path : Path.t -> context -> Module_var.t Module_access.t

  (* external *)
  val run : (context -> 'k) -> ('k, exn) result
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

  type t = context

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


  let run k =
    try Ok (k empty) with
    | exn -> Error exn


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


  (* TODO: drop all failwith *)
  (* TODO: reduce boilerplate below *)

  let rec solve_module_path path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (* TODO: use list not empty here *)
      (match Ident.Map.find_opt ident ctx.modules with
      | Some (module_, ctx) -> (module_, []), ctx
      | None -> failwith "unexpected module ident")
    | Pdot (left, right) ->
      let (rev_left_hd, rev_left_tl), ctx = solve_module_path left ctx in
      (match String_map.find_opt right ctx.md_modules with
      | Some (module_, ctx) -> (module_, rev_left_hd :: rev_left_tl), ctx
      | None -> failwith "unexpected module name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_value_path path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident ctx.values with
      | Some Value_external -> failwith "external value reached"
      | Some (Value_subst_to value) ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = value }
      | None -> failwith "unexpected value ident")
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), ctx = solve_module_path module_ ctx in
      (match String_map.find_opt right ctx.md_values with
      | Some Value_external -> failwith "external value reached"
      | Some (Value_subst_to value) ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = value }
      | None -> failwith "unexpected value name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_type_path path ctx =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident ctx.types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = type_ }
      | None -> failwith "unexpected type ident")
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), ctx = solve_module_path module_ ctx in
      (match String_map.find_opt right ctx.md_types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = type_ }
      | None -> failwith "unexpected type name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_module_path path ctx =
    let (rev_module_hd, rev_module_tl), ctx = solve_module_path path ctx in
    let module_path = List.rev rev_module_tl in
    Module_access.{ module_path; element = rev_module_hd }
end

open Context

(* TODO: solve is a bad word *)

(* TODO: either fully commit to combinators, or add expr_wrap and type_wrap *)
let pat_wrap location content : _ Ast_core.Pattern.t =
  Location.{ wrap_content = content; location }


let decl_wrap location content : Ast_core.decl =
  (* TODO: type *)
  { wrap_content = content; location }


let mod_expr_wrap location content : Ast_core.module_expr =
  (* TODO: type *)
  { wrap_content = content; location }


let sig_item_wrap location content : Ast_core.sig_item =
  (* TODO: type *)
  { wrap_content = content; location }


let sig_expr_wrap location content : Ast_core.signature_expr =
  (* TODO: type *)
  { wrap_content = content; location }


(* TODO: merge ctx and vars *)
let rec solve_type ctx vars typ_ =
  let { type_desc; type_loc = loc } = typ_ in
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
    t_variable ~loc var ()
  | T_constr (path, args) ->
    let type_operator = solve_type_path path ctx in
    (match args with
    | [] ->
      (* TODO: fix this on ligo *)
      (match type_operator with
      | { module_path = []; element } -> t_variable ~loc element ()
      | { module_path; element } as var -> t_module_accessor ~loc var ())
    | args ->
      (* TODO: what about no args? *)
      let arguments = List.map args ~f:(fun type_ -> solve_type ctx vars type_) in
      t_app ~loc { type_operator; arguments } ())
  | T_arrow (param, return) ->
    let param = solve_type ctx vars param in
    let return = solve_type ctx vars return in
    (* TODO: param names? *)
    t_arrow ~loc { type1 = param; type2 = return; param_names = [] } ()
  | T_tuple fields ->
    let fields = List.map fields ~f:(fun field -> solve_type ctx vars field) in
    t_record ~loc (Row.create_tuple fields) ()
  | T_forall (bound, body) ->
    (* TODO: this should fail if not poly *)
    (* TODO: test poly and univar *)
    solve_type_forall loc ctx vars ~bound body


and solve_type_forall loc ctx vars ~bound body =
  match bound with
  | [] -> solve_type ctx vars body
  | (name, id) :: bound ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_forall loc ctx vars ~bound body in
    Hashtbl.remove vars id;
    t_for_all ~loc { ty_binder = var; kind = Type; type_ = body } ()


let solve_type_poly ctx vars type_ =
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
  let { type_desc = _; type_loc = loc } = type_ in
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
      t_for_all ~loc { ty_binder = var; kind = Type; type_ = body } ())


let solve_type_record loc ctx vars fields =
  let fields =
    List.map fields ~f:(fun decl_label ->
        let { dl_id; dl_type; dl_loc } = decl_label in
        let label = Ident.name dl_id in
        let type_ = solve_type_poly ctx vars dl_type in
        Label.Label (label, dl_loc), type_)
  in
  let fields = Label.Map.of_alist_exn fields in
  (* TODO: layout *)
  let layout = None in
  t_record ~loc { fields; layout } ()


let rec solve_type_decl ctx vars decl : type_expression =
  let { type_decl_desc; type_decl_params; type_decl_loc = loc } = decl in
  solve_type_decl_lambda ctx vars loc type_decl_params type_decl_desc


and solve_type_decl_lambda ctx vars loc params desc =
  match params with
  | [] -> solve_type_decl_body ctx vars loc desc
  | (name, id) :: params ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_decl_lambda ctx vars loc params desc in
    Hashtbl.remove vars id;
    t_abstraction ~loc { ty_binder = var; kind = Type; type_ = body } ()


and solve_type_decl_body ctx vars loc desc =
  match desc with
  | T_record fields -> solve_type_record loc ctx vars fields
  | T_variant cases ->
    let cases =
      List.map cases ~f:(fun decl_case ->
          match decl_case with
          | C_tuple { dc_id; dc_fields; dc_loc } ->
            let label = Ident.name dc_id in
            let content =
              match dc_fields with
              | [] -> t_unit ~loc:dc_loc ()
              | [ field ] -> solve_type ctx vars field
              | fields ->
                let fields =
                  List.map fields ~f:(fun field -> solve_type ctx vars field)
                in
                t_record ~loc (Row.create_tuple fields) ()
            in
            Label.Label (label, dc_loc), content
          | C_record { dc_id; dc_fields; dc_loc } ->
            let label = Ident.name dc_id in
            let content = solve_type_record loc ctx vars dc_fields in
            Label.Label (label, dc_loc), content)
    in
    let cases =
      List.map cases ~f:(fun (label, content) ->
          let open Label in
          match label with
          | Label ("true", loc) -> Label ("True", loc), content
          | Label ("false", loc) -> Label ("False", loc), content
          | label -> label, content)
    in
    let cases = Label.Map.of_alist_exn cases in
    (* TODO: layout *)
    let layout = None in
    t_sum ~loc { fields = cases; layout } ()
  | T_alias manifest ->
    (* TODO: not poly tho *)
    solve_type_poly ctx vars manifest


let rec solve_pat ctx vars pat =
  let { pat_desc; pat_type; pat_loc = loc } = pat in
  (* TODO: ascription in all types *)
  (* TODO: dummy *)
  (* TODO: poly type *)
  let type_ = Some (solve_type ctx vars pat_type) in
  match pat_desc with
  | P_unit -> ctx, pat_wrap loc @@ P_unit
  | P_var ident ->
    let var = fresh_value ident in
    let binder = Binder.make var type_ in
    let ctx = enter_value ident var ctx in
    ctx, pat_wrap loc @@ P_var binder
  | P_tuple fields ->
    let ctx, rev_fields =
      List.fold_left fields ~init:(ctx, []) ~f:(fun (ctx, rev_fields) pat ->
          let ctx, field = solve_pat ctx vars pat in
          ctx, field :: rev_fields)
    in
    let fields = List.rev rev_fields in
    ctx, pat_wrap loc @@ P_tuple fields
  | P_record fields ->
    let ctx, rev_fields =
      List.fold_left fields ~init:(ctx, []) ~f:(fun (ctx, rev_fields) (label, pat) ->
          let ctx, pat = solve_pat ctx vars pat in
          ctx, (label, pat) :: rev_fields)
    in
    let fields = List.rev rev_fields in
    ctx, pat_wrap loc @@ P_record (Record.of_list fields)
  | P_variant (label, fields) ->
    (match label with
    (* TODO: this is very hackish *)
    | Label ("()", _loc) ->
      (* TODO: assert fields *)
      (* assert (List.is_empty fields); *)
      ctx, pat_wrap loc @@ P_unit
    (* | Label ("::", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      ctx, pat_wrap loc @@ E_constant { cons_name = C_CONS; arguments }
    | Label ("[]", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      ctx, pat_wrap loc @@ E_constant { cons_name = C_LIST_EMPTY; arguments } *)
    | label ->
      let ctx, fields = solve_pat ctx vars fields in
      ctx, pat_wrap loc @@ P_variant (label, fields))


let solve_var_pat ctx vars pat =
  let { var_pat_desc = ident; var_pat_type; var_pat_loc = loc } = pat in
  (* TODO: poly type *)
  let type_ = solve_type ctx vars var_pat_type in
  let var = fresh_value ident in
  let ctx = enter_value ident var ctx in
  ctx, loc, var, type_


let rec solve_expr ctx vars expr =
  let { expr_desc; expr_type; expr_loc = loc } = expr in
  (* TODO: use this expr_type *)
  match expr_desc with
  | E_var path ->
    let var = solve_value_path path ctx in
    (* TODO: fix this on ligo *)
    (match var with
    | { module_path = []; element } -> e_variable ~loc element
    | { module_path; element } as var -> e_module_accessor ~loc var ())
  | E_literal lit -> e_literal ~loc lit
  | E_let (pat, value, body) ->
    let inner_ctx, pat = solve_pat ctx vars pat in
    let value = solve_expr_poly ctx vars value in
    let body = solve_expr inner_ctx vars body in
    e_let_in ~loc pat value body Value_attr.default_attributes
  | E_lambda (param, body) ->
    let binder, body = solve_lambda ctx vars ~param ~body in
    (* TODO: output_type *)
    e_lambda ~loc binder None body
  | E_lambda_rec { self; param; body } ->
    let inner_ctx, _var_loc, self, self_type_ = solve_var_pat ctx vars param in
    (* ctx, loc, var, type_ *)
    let binder, return, body = solve_lambda_rec inner_ctx vars ~param ~body in
    (* TODO: output_type *)
    (* TODO: forcelambdarec *)
    let lambda = Lambda.{ binder; output_type = return; result = body } in
    e_recursive ~loc self self_type_ lambda
  | E_apply (lambda, args) ->
    let lambda = solve_expr ctx vars lambda in
    List.fold_left args ~init:lambda ~f:(fun lambda arg ->
        let arg = solve_expr ctx vars arg in
        e_application ~loc lambda arg)
  | E_match (matchee, cases) ->
    let matchee = solve_expr ctx vars matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let ctx, pat = solve_pat ctx vars pat in
          let body = solve_expr ctx vars body in
          Match_expr.{ pattern = pat; body })
    in
    e_matching ~loc matchee cases
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:(fun field -> solve_expr ctx vars field) in
    e_tuple ~loc fields ()
  | E_constructor (constructor, fields) ->
    (* TODO: high priority *)
    (match constructor with
    (* TODO: this is very hackish *)
    | Label ("()", _loc) ->
      assert (List.is_empty fields);
      e_literal ~loc Literal_unit
    | Label ("::", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      e_constant ~loc C_CONS arguments
    | Label ("[]", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      e_constant ~loc C_LIST_EMPTY arguments
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
      let element =
        match fields with
        | [] -> e_unit ~loc ()
        | [ field ] -> field
        | field :: fields -> e_tuple ~loc (field :: fields) ()
      in
      e_constructor ~loc constructor element)
  | E_record fields ->
    let fields =
      List.map fields ~f:(fun (label, field) -> label, solve_expr ctx vars field)
    in
    e_record ~loc (Record.of_list fields) ()
  | E_field (struct_, label) ->
    let struct_ = solve_expr ctx vars struct_ in
    e_accessor ~loc { struct_; path = label } ()


and solve_expr_poly ctx vars expr =
  (* TODO: this is also duplicated *)
  (* TODO: this is bad *)
  let { expr_desc = _; expr_type = _; expr_loc = loc } = expr in
  let external_vars = Hashtbl.copy vars in
  (* TODO: looks weird to extract here *)
  let expr = solve_expr ctx vars expr in
  let internal_vars =
    List.filter_map (Hashtbl.to_alist vars) ~f:(fun (id, var) ->
        match Hashtbl.mem external_vars id with
        | true -> None
        | false ->
          Hashtbl.remove vars id;
          Some var)
  in
  List.fold_left internal_vars ~init:expr ~f:(fun body var ->
      e_type_abstraction ~loc { type_binder = var; result = body } ())


and solve_lambda ctx vars ~param ~body =
  let inner_ctx, _var_loc, var, var_type_ = solve_var_pat ctx vars param in
  (* TODO: {mut,forced,initial} flag *)
  let binder = Param.make var (Some var_type_) in
  let body = solve_expr inner_ctx vars body in
  binder, body


and solve_lambda_rec ctx vars ~param ~body =
  let inner_ctx, _var_loc, var, var_type_ = solve_var_pat ctx vars param in
  (* TODO: {mut,forced,initial} flag *)
  let binder = Param.make var var_type_ in
  let return =
    (* TODO: this is hackish *)
    let { expr_desc = _; expr_type = return; expr_loc = _ } = body in
    solve_type ctx vars return
  in
  let body = solve_expr inner_ctx vars body in
  binder, return, body


let rec solve_module ctx module_ =
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
  let { decl_desc; decl_loc = loc } = decl in
  match decl_desc with
  | D_let (var_pat, value) ->
    (* TODO: duplicated logic regarding var_pat and Binder *)
    (* TODO: use var_pat_loc *)
    let { var_pat_desc = ident; var_pat_type; var_pat_loc = _ } = var_pat in
    let var = fresh_value ident in
    let type_ = solve_type_poly ctx vars var_pat_type in
    let binder = Binder.make var (Some type_) in
    let value = solve_expr_poly ctx vars value in
    let ctx = enter_value ident var ctx in
    let attr = Value_attr.default_attributes in
    ctx, Some (decl_wrap loc @@ D_value { binder; expr = value; attr })
  | D_type (ident, type_decl) ->
    let type_decl = solve_type_decl ctx vars type_decl in
    let var = fresh_type ident in
    let ctx = Context.enter_type ident var ctx in
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_type
             { type_binder = var
             ; type_expr = type_decl
             ; type_attr = Type_or_module_attr.default_attributes
             }) )
  | D_external ident ->
    let ctx = enter_value_external ident ctx in
    ctx, None
  | D_type_predef (ident, literal, arity) ->
    (* TODO: this is brittle, what if duplicated? *)
    let var = Type_var.of_input_var ~loc @@ Literal_types.to_string @@ literal in
    let ctx = enter_type ident var ctx in
    let type_decl =
      (* TODO: lacking t_constant *)
      make_t ~loc @@ T_constant (literal, arity)
    in
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_type
             { type_binder = var
             ; type_expr = type_decl
             ; type_attr = Type_or_module_attr.default_attributes
             }) )
  | D_type_unsupported ident ->
    let ctx = enter_type_predef_unsupported ident ctx in
    ctx, None
  | D_module (ident, mod_expr) ->
    let var = fresh_module ident in
    let ctx, module_ =
      enter_module ident var ctx @@ fun ctx -> solve_mod_expr ctx mod_expr
    in
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_module
             { module_binder = var
             ; module_ (* TODO: annotation *)
             ; annotation = None
             ; module_attr = TypeOrModuleAttr.default_attributes
             }) )
  | D_module_type (ident, sig_expr) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_signature ident var ctx @@ fun ctx -> solve_sig_expr ctx sig_expr
    in
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_signature
             { signature_binder = var
             ; signature
             ; signature_attr = Signature_attr.default_attributes
             }) )


and solve_mod_expr ctx mod_expr =
  let { mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_var path ->
    (* TODO: improve this on Ligo *)
    (match solve_module_path path ctx with
    | { module_path = []; element } -> ctx, mod_expr_wrap loc @@ M_variable element
    | { module_path; element } ->
      (* TODO: is this reversing it? *)
      ctx, mod_expr_wrap loc @@ M_module_path (element :: module_path))
  | M_struct decls ->
    let ctx, decls = solve_module ctx decls in
    ctx, mod_expr_wrap loc @@ M_struct decls


and solve_signature ctx sig_ =
  (* TODO: duplicated from solve module *)
  let ctx, rev_sig =
    List.fold_left sig_ ~init:(ctx, []) ~f:(fun (ctx, rev_sig) decl ->
        let ctx, sigi = solve_sigi ctx decl in
        (* TODO: this is ugly *)
        match sigi with
        | None -> ctx, rev_sig
        | Some sigi -> ctx, sigi :: rev_sig)
  in
  ctx, { items = List.rev rev_sig }


and solve_sigi ctx sigi =
  (* TODO: duplicated from solve_decl *)
  let vars = Hashtbl.create (module Int) in
  let ctx, sigi = solve_sigi_inner ctx vars sigi in
  assert (Hashtbl.is_empty vars);
  ctx, sigi


and solve_sigi_inner ctx vars sigi =
  let { sig_item_desc; sig_item_loc = loc } = sigi in
  match sig_item_desc with
  | S_value (ident, type_) ->
    let var = fresh_value ident in
    let type_ = solve_type_poly ctx vars type_ in
    let ctx = enter_value ident var ctx in
    let attr = SigItemAttr.default_attributes in
    ctx, Some (sig_item_wrap loc @@ S_value (var, type_, attr))
  | S_type (ident, type_decl) ->
    let type_decl = solve_type_decl ctx vars type_decl in
    let var = fresh_type ident in
    let ctx = enter_type ident var ctx in
    let attr = SigTypeAttr.default_attributes in
    ctx, Some (sig_item_wrap loc @@ S_type (var, type_decl, attr))
  | S_module (ident, mod_sig) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_module ident var ctx @@ fun ctx -> solve_signature ctx mod_sig
    in
    (* TODO: use this inner_ctx? *)
    ctx, Some (sig_item_wrap loc @@ S_module (var, signature))
  | S_module_type (ident, signature) ->
    let var = fresh_module ident in
    let ctx, signature =
      enter_module ident var ctx @@ fun ctx -> solve_signature ctx signature
    in
    ctx, Some (sig_item_wrap loc @@ S_module_type (var, signature))


and solve_sig_expr ctx sig_expr =
  let { sig_expr_desc; sig_expr_loc = loc } = sig_expr in
  match sig_expr_desc with
  | S_var path ->
    let path = solve_module_path path ctx in
    (* TODO: is this reversing it? *)
    let Module_access.{ module_path; element } = path in
    ctx, sig_expr_wrap loc @@ S_path (element :: module_path)
  | S_sig signature ->
    let ctx, signature = solve_signature ctx signature in
    ctx, sig_expr_wrap loc @@ S_sig signature
