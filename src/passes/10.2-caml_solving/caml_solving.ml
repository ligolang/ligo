open Ocaml_common
open Ligo_prim
open Ast_core
open Caml_core

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
  val enter_type : Ident.t -> Type_var.t -> context -> context
  val enter_type_predef_unsupported : Ident.t -> context -> context

  val enter_module
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

  type type_info =
    | Type_unsupported_predef
    | Type_subst_to of Type_var.t

  type md_context =
    { md_values : Value_var.t String_map.t
    ; md_types : type_info String_map.t
    ; md_modules : (Module_var.t * md_context) String_map.t
    }

  (* TODO: try with in all OCaml functions *)
  type context =
    { values : Value_var.t Ident.Map.t
    ; types : type_info Ident.Map.t
    ; modules : (Module_var.t * md_context) Ident.Map.t
    ; local : md_context
    }

  type t = context

  let empty_local =
    { md_values = String_map.empty
    ; md_types = String_map.empty
    ; md_modules = String_map.empty
    }


  let empty =
    { values = Ident.Map.empty
    ; types = Ident.Map.empty
    ; modules = Ident.Map.empty
    ; local = empty_local
    }


  let run k =
    try Ok (k empty) with
    | exn -> Error exn


  let enter_value ident value_var ctx =
    let name = Ident.name ident in
    let values = Ident.Map.add ident value_var ctx.values in
    let md_values = String_map.add name value_var ctx.local.md_values in
    { ctx with values; local = { ctx.local with md_values } }


  let enter_type_info ident type_info ctx =
    let { values = _; types; modules = _; local } = ctx in
    let name = Ident.name ident in
    let types = Ident.Map.add ident type_info types in
    let md_types = String_map.add name type_info ctx.local.md_types in
    { ctx with types; local = { ctx.local with md_types } }


  let enter_module ident module_var ctx k =
    let { values = _; types = _; modules; local } = ctx in
    let inner_ctx, x = k { ctx with local = empty_local } in
    let name = Ident.name ident in
    let mod_data = module_var, inner_ctx.local in
    let modules = Ident.Map.add ident mod_data modules in
    let local =
      let { md_values = _; md_types = _; md_modules } = local in
      let md_modules = String_map.add name mod_data md_modules in
      { local with md_modules }
    in
    { ctx with modules; local }, x


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
      | Some value ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = value }
      | None -> failwith "unexpected value ident")
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), ctx = solve_module_path module_ ctx in
      (match String_map.find_opt right ctx.md_values with
      | Some value ->
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
let type_wrap location content = { type_content = content; location }

let pat_wrap location content : _ Ast_core.Pattern.t =
  Location.{ wrap_content = content; location }


(* TODO: use Ligo e_stuff function *)
let expr_wrap location content =
  (* TODO: type *)
  { expression_content = content; location }


let decl_wrap location content : Ast_core.decl =
  (* TODO: type *)
  { wrap_content = content; location }


let mod_expr_wrap location content : Ast_core.module_expr =
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
    type_wrap loc @@ T_variable var
  | T_constr (path, args) ->
    let type_operator = solve_type_path path ctx in
    (match args with
    | [] ->
      (* TODO: fix this on ligo *)
      (match type_operator with
      | { module_path = []; element } -> type_wrap loc @@ T_variable element
      | { module_path; element } as var -> type_wrap loc @@ T_module_accessor var)
    | args ->
      (* TODO: what about no args? *)
      let arguments = List.map args ~f:(fun type_ -> solve_type ctx vars type_) in
      type_wrap loc @@ T_app { type_operator; arguments })
  | T_arrow (param, return) ->
    let param = solve_type ctx vars param in
    let return = solve_type ctx vars return in
    (* TODO: param names? *)
    type_wrap loc @@ T_arrow { type1 = param; type2 = return; param_names = [] }
  | T_tuple fields ->
    let fields = List.map fields ~f:(fun field -> solve_type ctx vars field) in
    type_wrap loc @@ T_record (Row.create_tuple fields)
  | T_forall (bound, body) ->
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
    type_wrap loc @@ T_for_all { ty_binder = var; kind = Type; type_ = body }


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
      type_wrap loc @@ T_for_all { ty_binder = var; kind = Type; type_ = body })


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
  type_wrap loc @@ T_record { fields; layout }


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
    type_wrap loc @@ T_abstraction { ty_binder = var; kind = Type; type_ = body }


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
                type_wrap loc @@ T_record (Row.create_tuple fields)
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
    type_wrap loc @@ T_sum { fields = cases; layout }
  | T_alias manifest ->
    (* TODO: not poly tho *)
    solve_type_poly ctx vars manifest


(* TODO: solve_var_pat *)

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


let rec solve_expr ctx vars expr =
  let { expr_desc; expr_type; expr_loc = loc } = expr in
  match expr_desc with
  | E_var path ->
    let var = solve_value_path path ctx in
    (* TODO: fix this on ligo *)
    (match var with
    | { module_path = []; element } -> expr_wrap loc @@ E_variable element
    | { module_path; element } as var -> expr_wrap loc @@ E_module_accessor var)
  | E_literal lit -> expr_wrap loc @@ E_literal lit
  | E_let (pat, value, body) ->
    let inner_ctx, pat = solve_pat ctx vars pat in
    let value = solve_expr_poly ctx vars value in
    let body = solve_expr inner_ctx vars body in
    expr_wrap loc
    @@ E_let_in
         { let_binder = pat
         ; rhs = value
         ; let_result = body
         ; attributes = Value_attr.default_attributes
         }
  | E_let_rec _ ->
    (* TODO: high priority *)
    error_unimplemented ()
  | E_lambda (var_pat, body) ->
    (* TODO: use var_pat_loc *)
    let { var_pat_desc; var_pat_type; var_pat_loc = _ } = var_pat in
    let var = fresh_value var_pat_desc in
    let ctx = enter_value var_pat_desc var ctx in
    let body = solve_expr ctx vars body in
    let binder =
      (* TODO: {mut,forced,initial} flag *)
      let param_annot = solve_type ctx vars var_pat_type in
      Param.make var (Some param_annot)
    in
    (* TODO: output_type *)
    expr_wrap loc @@ E_lambda { binder; output_type = None; result = body }
  | E_apply (lambda, args) ->
    let lambda = solve_expr ctx vars lambda in
    List.fold_left args ~init:lambda ~f:(fun lambda arg ->
        let arg = solve_expr ctx vars arg in
        expr_wrap loc @@ E_application { lamb = lambda; args = arg })
  | E_match (matchee, cases) ->
    let matchee = solve_expr ctx vars matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let ctx, pat = solve_pat ctx vars pat in
          let body = solve_expr ctx vars body in
          Match_expr.{ pattern = pat; body })
    in
    expr_wrap loc @@ E_matching { matchee; cases }
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:(fun field -> solve_expr ctx vars field) in
    expr_wrap loc @@ E_tuple fields
  | E_constructor (constructor, fields) ->
    (* TODO: high priority *)
    (match constructor with
    (* TODO: this is very hackish *)
    | Label ("()", _loc) ->
      assert (List.is_empty fields);
      expr_wrap loc @@ E_literal Literal_unit
    | Label ("::", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      expr_wrap loc @@ E_constant { cons_name = C_CONS; arguments }
    | Label ("[]", _loc) ->
      let arguments = List.map fields ~f:(fun field -> solve_expr ctx vars field) in
      expr_wrap loc @@ E_constant { cons_name = C_LIST_EMPTY; arguments }
    | _ ->
      (* TODO: location? *)
      let constructor =
        let open Label in
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
      expr_wrap loc @@ E_constructor { constructor; element })
  | E_record fields ->
    let fields =
      List.map fields ~f:(fun (label, field) ->
          (* TODO: location *)
          Label.Label (label, Location.dummy), solve_expr ctx vars field)
    in
    expr_wrap loc @@ E_record (Record.of_list fields)
  | E_field (struct_, label) ->
    let struct_ = solve_expr ctx vars struct_ in
    let path = Label.Label (label, Location.dummy) in
    expr_wrap loc @@ E_accessor { struct_; path }


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
      expr_wrap loc @@ E_type_abstraction { type_binder = var; result = body })


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
  | D_value { var = var_pat; value; entry } ->
    (* TODO: duplicated logic regarding var_pat and Binder *)
    (* TODO: use var_pat_loc *)
    let { var_pat_desc = ident; var_pat_type; var_pat_loc = _ } = var_pat in
    let var = fresh_value ident in
    let type_ = solve_type_poly ctx vars var_pat_type in
    let binder = Binder.make var (Some type_) in
    let value = solve_expr_poly ctx vars value in
    let ctx = Context.enter_value ident var ctx in
    (* TODO: returning none is weird *)
    let attr = { ValueAttr.default_attributes with entry } in
    ctx, Some (decl_wrap loc @@ D_value { binder; expr = value; attr })
  | D_value_rec _ ->
    (* TODO: high priority *)
    error_unimplemented ()
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
  | D_type_predef (ident, literal, arity) ->
    (* TODO: this is brittle, what if duplicated? *)
    let var = Type_var.of_input_var ~loc @@ Literal_types.to_string @@ literal in
    let ctx = Context.enter_type ident var ctx in
    let type_decl = type_wrap loc @@ T_constant (literal, arity) in
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_type
             { type_binder = var
             ; type_expr = type_decl
             ; type_attr = Type_or_module_attr.default_attributes
             }) )
  | D_type_unsupported ident ->
    let ctx = Context.enter_type_predef_unsupported ident ctx in
    ctx, None
  | D_module (ident, mod_expr) ->
    let var = fresh_module ident in
    let ctx, module_ =
      Context.enter_module ident var ctx @@ fun ctx -> solve_mod_expr ctx mod_expr
    in
    (* TODO: use this inner_ctx? *)
    ( ctx
    , Some
        (decl_wrap loc
        @@ D_module
             { module_binder = var
             ; module_ (* TODO: annotation *)
             ; annotation = None
             ; module_attr = TypeOrModuleAttr.default_attributes
             }) )


and solve_mod_expr ctx mod_expr =
  let { mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_struct decls ->
    let ctx, decls = solve_module ctx decls in
    ctx, mod_expr_wrap loc @@ M_struct decls
  | M_var path ->
    (* TODO: improve this on Ligo *)
    (match Context.solve_module_path path ctx with
    | { module_path = []; element } -> ctx, mod_expr_wrap loc @@ M_variable element
    | { module_path; element } ->
      ctx, mod_expr_wrap loc @@ M_module_path (element :: module_path))
