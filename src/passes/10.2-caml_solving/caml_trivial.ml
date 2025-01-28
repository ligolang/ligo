(* TODO: this just happens because there is no errors in Ast_core *)

open Ligo_prim
open Ast_core
open Caml_pre_core
open Caml_error

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


let rec lower_type typ_ =
  let { type_desc; type_loc = loc } = typ_ in
  match type_desc with
  | T_var var -> t_variable ~loc var ()
  | T_constr (path, args) ->
    (match args with
    | [] ->
      (* TODO: fix this on ligo *)
      (match path with
      | { module_path = []; element } -> t_variable ~loc element ()
      | { module_path; element } as var -> t_module_accessor ~loc var ())
    | args ->
      (* TODO: what about no args? *)
      let arguments = List.map args ~f:lower_type in
      t_app ~loc { type_operator = path; arguments } ())
  | T_arrow (param, return) ->
    let param = lower_type param in
    let return = lower_type return in
    (* TODO: param names? *)
    t_arrow ~loc { type1 = param; type2 = return; param_names = [] } ()
  | T_tuple fields ->
    let fields = List.map fields ~f:lower_type in
    t_record ~loc (Row.create_tuple fields) ()
  | T_forall (var, body) ->
    let body = lower_type body in
    t_for_all ~loc { ty_binder = var; kind = Type; type_ = body } ()
  | T_error error -> raise_error error


let rec lower_type_decl decl =
  let { type_decl_desc = body; type_decl_params = params; type_decl_loc = loc } = decl in
  let body = lower_type_decl_body loc body in
  List.fold_right params ~init:body ~f:(fun var body ->
      t_abstraction ~loc { ty_binder = var; kind = Type; type_ = body } ())


and lower_type_decl_body loc desc =
  match desc with
  | T_record fields -> lower_type_record loc fields
  | T_variant cases ->
    let cases =
      Label.Map.map cases ~f:(fun decl_case ->
          match decl_case with
          | C_tuple { dc_id; dc_fields; dc_loc } ->
            (match dc_fields with
            | [] -> t_unit ~loc:dc_loc ()
            | [ field ] -> lower_type field
            | fields ->
              let fields = List.map fields ~f:lower_type in
              t_record ~loc (Row.create_tuple fields) ())
          | C_record { dc_id; dc_fields; dc_loc } -> lower_type_record loc dc_fields)
    in
    (* TODO: layout *)
    let layout = None in
    t_sum ~loc { fields = cases; layout } ()
  | T_alias manifest ->
    (* TODO: not poly tho *)
    lower_type manifest
  | T_error error -> raise_error error


and lower_type_record loc fields =
  let fields = Label.Map.map fields ~f:lower_type in
  (* TODO: layout *)
  let layout = None in
  t_record ~loc { fields; layout } ()


let rec lower_pat pat =
  let { pat_desc; pat_type; pat_loc = loc } = pat in
  match pat_desc with
  | P_unit -> pat_wrap loc @@ P_unit
  | P_var var ->
    let type_ = Some (lower_type pat_type) in
    let binder = Binder.make var type_ in
    pat_wrap loc @@ P_var binder
  | P_tuple fields ->
    let fields = List.map fields ~f:lower_pat in
    pat_wrap loc @@ P_tuple fields
  | P_record fields ->
    let fields = Label.Map.map fields ~f:lower_pat in
    pat_wrap loc @@ P_record fields
  | P_variant (label, fields) ->
    let fields = lower_pat fields in
    pat_wrap loc @@ P_variant (label, fields)
  | P_error error -> raise_error error


let lower_var_pat pat =
  let { var_pat_desc; var_pat_type; var_pat_loc = loc } = pat in
  match var_pat_desc with
  | VP_var var ->
    let type_ = lower_type var_pat_type in
    var, type_
  | VP_error error -> raise_error error


let wrap_foralls ~loc foralls value =
  List.fold_right foralls ~init:value ~f:(fun var body ->
      e_type_abstraction ~loc { type_binder = var; result = body } ())


let rec lower_expr expr =
  let { expr_desc; expr_type; expr_loc = loc } = expr in
  match expr_desc with
  | E_var var ->
    (* TODO: fix this on ligo *)
    (match var with
    | { module_path = []; element } -> e_variable ~loc element
    | { module_path; element } as var -> e_module_accessor ~loc var ())
  | E_literal lit -> e_literal ~loc lit
  | E_constant { cons_name; arguments } ->
    let arguments = List.map arguments ~f:lower_expr in
    e_constant ~loc cons_name arguments
  | E_let { binder; foralls; attr; value; body } ->
    (* TODO: E_type_abstraction *)
    let binder = lower_pat binder in
    let value = wrap_foralls ~loc foralls @@ lower_expr value in
    let body = lower_expr body in
    e_let_in ~loc binder value body attr
  | E_let_module (var, mod_expr, body) ->
    let mod_expr = lower_mod_expr mod_expr in
    let body = lower_expr body in
    e_mod_in ~loc var mod_expr body
  | E_lambda (param, body) ->
    let binder =
      let var, type_ = lower_var_pat param in
      Param.make var (Some type_)
    in
    let body = lower_expr body in
    e_lambda ~loc binder None body
  | E_lambda_rec { self; param; body } ->
    let self, self_type = lower_var_pat self in
    let param =
      let var, type_ = lower_var_pat param in
      Param.make var type_
    in
    let return =
      (* TODO: this is hackish *)
      let { expr_desc = _; expr_type = return; expr_loc = _ } = body in
      lower_type return
    in
    let body = lower_expr body in
    let lambda = Lambda.{ binder = param; output_type = return; result = body } in
    e_recursive ~loc self self_type lambda
  | E_apply (lambda, args) ->
    let lambda = lower_expr lambda in
    List.fold_left args ~init:lambda ~f:(fun lambda arg ->
        let arg = lower_expr arg in
        e_application ~loc lambda arg)
  | E_match (matchee, cases) ->
    let matchee = lower_expr matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let pat = lower_pat pat in
          let body = lower_expr body in
          Match_expr.{ pattern = pat; body })
    in
    e_matching ~loc matchee cases
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:lower_expr in
    e_tuple ~loc fields ()
  | E_constructor (constructor, fields) ->
    (* TODO: location? *)
    let fields = List.map fields ~f:lower_expr in
    let element =
      match fields with
      | [] -> e_unit ~loc ()
      | [ field ] -> field
      | field :: fields -> e_tuple ~loc (field :: fields) ()
    in
    e_constructor ~loc constructor element
  | E_record fields ->
    let fields = Label.Map.map fields ~f:lower_expr in
    e_record ~loc fields ()
  | E_field (struct_, label) ->
    let struct_ = lower_expr struct_ in
    e_accessor ~loc { struct_; path = label } ()
  | E_error error -> raise_error error


and lower_module module_ = List.filter_map module_ ~f:lower_decl

and lower_decl decl =
  let { decl_desc; decl_loc = loc } = decl in
  match decl_desc with
  | D_let { binder; foralls; attr; value } ->
    let binder =
      let var, type_ = lower_var_pat binder in
      Binder.make var (Some type_)
    in
    let value = wrap_foralls ~loc foralls @@ lower_expr value in
    Some (decl_wrap loc @@ D_value { binder; expr = value; attr })
  | D_type (var, attr, type_decl) ->
    let type_decl = lower_type_decl type_decl in
    Some
      (decl_wrap loc
      @@ D_type { type_binder = var; type_expr = type_decl; type_attr = attr })
  | D_module (var, attr, mod_expr) ->
    let module_ = lower_mod_expr mod_expr in
    Some
      (decl_wrap loc
      @@ D_module
           { module_binder = var
           ; module_ (* TODO: annotation *)
           ; annotation = None
           ; module_attr = attr
           })
  | D_module_include mod_expr ->
    let mod_expr = lower_mod_expr mod_expr in
    Some (decl_wrap loc @@ D_module_include mod_expr)
  | D_module_type (var, attr, sig_expr) ->
    let signature = lower_sig_expr sig_expr in
    Some
      (decl_wrap loc
      @@ D_signature { signature_binder = var; signature; signature_attr = attr })
  | D_type_predef (var, literal, arity) ->
    (* TODO: maybe this should be a special type_decl instead? *)
    let type_decl =
      (* TODO: lacking t_constant *)
      make_t ~loc @@ T_constant (literal, arity)
    in
    (* TODO; attributes here? *)
    Some
      (decl_wrap loc
      @@ D_type
           { type_binder = var
           ; type_expr = type_decl
           ; type_attr = Type_or_module_attr.default_attributes
           })
  | D_type_unsupported -> None
  | D_attribute -> None
  | D_error error -> raise_error error


and lower_mod_expr mod_expr =
  let { mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_var path ->
    (* TODO: improve this on Ligo *)
    (match path with
    | { module_path = []; element } -> mod_expr_wrap loc @@ M_variable element
    | { module_path; element } ->
      (* TODO: is this reversing it? *)
      mod_expr_wrap loc @@ M_module_path (element :: module_path))
  | M_struct decls ->
    let decls = lower_module decls in
    mod_expr_wrap loc @@ M_struct decls


and lower_signature sig_ = { items = List.map sig_ ~f:lower_sigi }

and lower_sigi sigi =
  let { sig_item_desc; sig_item_loc = loc } = sigi in
  match sig_item_desc with
  | S_value (var, attr, type_) ->
    let type_ = lower_type type_ in
    sig_item_wrap loc @@ S_value (var, type_, attr)
  | S_type (var, attr, type_decl) ->
    let type_decl = lower_type_decl type_decl in
    sig_item_wrap loc @@ S_type (var, type_decl, attr)
  | S_module (var, mod_sig) ->
    let signature = lower_signature mod_sig in
    (* TODO: use this inner_ctx? *)
    sig_item_wrap loc @@ S_module (var, signature)
  | S_module_type (var, signature) ->
    let signature = lower_signature signature in
    sig_item_wrap loc @@ S_module_type (var, signature)
  | S_error error -> raise_error error


and lower_sig_expr sig_expr =
  let { sig_expr_desc; sig_expr_loc = loc } = sig_expr in
  match sig_expr_desc with
  | S_var path ->
    (* TODO: is this reversing it? *)
    let Module_access.{ module_path; element } = path in
    sig_expr_wrap loc @@ S_path (element :: module_path)
  | S_sig signature ->
    let signature = lower_signature signature in
    sig_expr_wrap loc @@ S_sig signature


let lower_module module_ =
  (* TODO: proper location here *)
  let loc = Location.dummy in
  wrap_exn ~loc (fun () -> lower_module module_)
