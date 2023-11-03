module Ligo_string = Simple_utils.Ligo_string
module Location = Simple_utils.Location
module I = Ast_core
module O = Ast_typed
open Ligo_prim

let untype_value_attr : O.ValueAttr.t -> I.ValueAttr.t = fun x -> x

let untype_sig_item_attr : O.sig_item_attribute -> I.sig_item_attribute =
 fun { entry; dyn_entry; view; optional } -> { entry; view; dyn_entry; optional }


(* use_orig_var param allows us to preserve the orininal type variables names, e.g. if
   we have [type t = A | B] then type of [Some A] will be transformed to [t option]
  instead of default [(A | B) option].
  It needs to be done during untyping, since O.type_expression can have orig_var
  and I.type_expression can't *)
let rec untype_type_expression ?(use_orig_var = false) (t : O.type_expression)
    : I.type_expression
  =
  let loc = t.location in
  let self = untype_type_expression ~use_orig_var in
  let return t = I.make_t ~loc t in
  return
  @@
  match t.orig_var with
  | Some orig_var when use_orig_var -> I.T_variable orig_var
  | _ ->
    (match t.type_content with
    | O.T_sum { fields; layout } ->
      let fields = Map.map fields ~f:self in
      I.T_sum { fields; layout = Some layout }
    | O.T_record { fields; layout } ->
      let fields = Map.map fields ~f:self in
      I.T_record { fields; layout = Some layout }
    | O.T_variable name -> I.T_variable name
    | O.T_arrow arr ->
      let arr = Arrow.map self arr in
      I.T_arrow arr
    | O.T_constant { language = _; injection; parameters } ->
      let arguments = List.map ~f:self parameters in
      let type_operator =
        Type_var.of_input_var ~loc (Literal_types.to_string injection)
      in
      (match arguments with
      | [] -> I.T_variable type_operator
      | _ -> I.T_app { type_operator = Module_access.make_el @@ type_operator; arguments })
    | O.T_singleton l -> I.T_singleton l
    | O.T_abstraction x ->
      let x = Abstraction.map self x in
      T_abstraction x
    | O.T_for_all x ->
      let x = Abstraction.map self x in
      T_for_all x)


let untype_type_expression_option x = Option.return @@ untype_type_expression x

let rec untype_expression (e : O.expression) : I.expression =
  untype_expression_content ~loc:e.location e.expression_content


and untype_expression_content ~loc (ec : O.expression_content) : I.expression =
  let open I in
  let self = untype_expression in
  let self_type = untype_type_expression in
  let self_type_opt = untype_type_expression_option in
  let return e = e in
  match ec with
  | E_literal l -> return (e_literal ~loc l)
  | E_contract x -> return (e_contract ~loc x ())
  | E_constant { cons_name; arguments } ->
    let lst' = List.map ~f:self arguments in
    return (e_constant ~loc cons_name lst')
  | E_variable n -> return (e_variable ~loc n)
  | E_application { lamb; args } ->
    let f' = self lamb in
    let arg' = self args in
    return (e_application ~loc f' arg')
  | E_lambda { binder; output_type; result } ->
    let binder = Param.map self_type_opt binder in
    let output_type = self_type_opt output_type in
    let result = self result in
    return (e_lambda ~loc binder output_type result)
  | E_type_abstraction { type_binder; result } ->
    let result = self result in
    return (e_type_abs ~loc type_binder result)
  | E_constructor { constructor; element } ->
    let p' = self element in
    return (e_constructor ~loc constructor p')
  | E_record r ->
    let r' = Record.map ~f:self r in
    return (e_record ~loc r' ())
  | E_accessor { struct_; path } ->
    let r' = self struct_ in
    let (Label s) = path in
    return (e_record_accessor ~loc r' (Label s))
  | E_update { struct_ = r; path = Label l; update = e } ->
    let r' = self r in
    let e = self e in
    return (e_record_update ~loc r' (Label l) e)
  | E_matching m ->
    let I.Match_expr.{ matchee; cases } = untype_match_expr m in
    return (e_matching ~loc matchee cases)
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let result = self let_result in
    let attr : ValueAttr.t = untype_value_attr attributes in
    let let_binder = O.Pattern.map (Fn.const None) let_binder in
    return (e_let_mut_in ~loc let_binder rhs result attr)
  | E_mod_in { module_binder; rhs; let_result } ->
    let rhs = untype_module_expr rhs in
    let result = self let_result in
    return @@ e_mod_in ~loc module_binder rhs result
  | E_raw_code { language; code } ->
    let code = self code in
    return (e_raw_code ~loc language code)
  | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
    let fun_type = self_type fun_type in
    let lambda = Lambda.map self self_type lambda in
    return @@ e_recursive ~loc ~force_lambdarec fun_name fun_type lambda
  | E_module_accessor ma -> return @@ I.make_e ~loc @@ E_module_accessor ma
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let result = self let_result in
    let attr : ValueAttr.t = untype_value_attr attributes in
    let let_binder = O.Pattern.map (Fn.const None) let_binder in
    return (e_let_in ~loc let_binder rhs result attr)
  | E_assign a ->
    let a = Assign.map self self_type_opt a in
    return @@ make_e ~loc @@ E_assign a
  | E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ I.make_e ~loc @@ E_for for_loop
  | E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ I.make_e ~loc @@ E_for_each for_each_loop
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ I.make_e ~loc @@ E_while while_loop
  | E_deref var -> return @@ I.make_e ~loc @@ E_variable var
  | E_coerce asc ->
    let asc = Ascription.map self self_type asc in
    return @@ I.make_e ~loc @@ E_ascription asc
  | E_type_inst { forall; type_ = type_inst } ->
    (match forall.type_expression.type_content with
    | T_for_all { ty_binder; type_; kind = _ } ->
      let type_ = Ast_typed.Helpers.subst_type ty_binder type_inst type_ in
      let forall = { forall with type_expression = type_ } in
      self forall
    | T_arrow _ ->
      (* This case is used for external typers *)
      self forall
    | _ ->
      failwith "Impossible case: cannot untype a type instance of a non polymorphic type")


and untype_match_expr
    :  (O.expression, O.type_expression) O.Match_expr.t
    -> (I.expression, I.type_expression option) I.Match_expr.t
  =
 fun { matchee; cases } ->
  let matchee = untype_expression matchee in
  let cases =
    List.map cases ~f:(fun { pattern; body } ->
        let pattern = O.Pattern.map untype_type_expression_option pattern in
        let body = untype_expression body in
        I.Match_expr.{ pattern; body })
  in
  I.Match_expr.{ matchee; cases }


(* TODO: check usage *)
and untype_pattern : _ O.Pattern.t -> _ I.Pattern.t =
 fun p ->
  let self = untype_pattern in
  let loc = Location.get_location p in
  match Location.unwrap p with
  | P_unit -> Location.wrap ~loc I.Pattern.P_unit
  | P_var b -> Location.wrap ~loc (I.Pattern.P_var b)
  | P_list (Cons (h, t)) ->
    let h = self h in
    let t = self t in
    Location.wrap ~loc (I.Pattern.P_list (Cons (h, t)))
  | P_list (List ps) ->
    let ps = List.map ~f:self ps in
    Location.wrap ~loc (I.Pattern.P_list (List ps))
  | P_variant (l, p) ->
    let p = self p in
    Location.wrap ~loc (I.Pattern.P_variant (l, p))
  | P_tuple ps ->
    let ps = List.map ~f:self ps in
    Location.wrap ~loc (I.Pattern.P_tuple ps)
  | P_record lps ->
    let lps = Record.map ~f:self lps in
    Location.wrap ~loc (I.Pattern.P_record lps)


and untype_module_expr : O.module_expr -> I.module_expr =
 fun module_expr ->
  let loc = module_expr.module_location in
  let return wrap_content : I.module_expr = Location.wrap ~loc wrap_content in
  match module_expr.module_content with
  | M_struct prg ->
    let prg = untype_module prg in
    return (M_struct prg)
  | M_module_path path -> return (M_module_path path)
  | M_variable v -> return (M_variable v)


and untype_sig_item ?(use_orig_var = false) : O.sig_item -> I.sig_item =
 fun sig_item ->
  match sig_item with
  | S_value (var, type_, attr) ->
    S_value (var, untype_type_expression ~use_orig_var type_, untype_sig_item_attr attr)
  | S_type (var, type_) when Option.is_some @@ type_.orig_var ->
    (* we do not want to print the original variable if that is the first definition or an alias *)
    S_type (var, untype_type_expression type_)
  | S_type (var, type_) -> S_type (var, untype_type_expression ~use_orig_var type_)
  | S_type_var var -> S_type_var var
  | S_module (var, sig_) -> S_module (var, untype_signature ~use_orig_var sig_)
  | S_module_type (var, sig_) -> S_module_type (var, untype_signature ~use_orig_var sig_)


and untype_signature ?(use_orig_var = false) : O.signature -> I.signature =
 fun signature ->
  { items = List.map ~f:(untype_sig_item ~use_orig_var) signature.sig_items }


and untype_declaration_constant
    : (O.expression -> I.expression) -> _ O.Value_decl.t -> _ I.Value_decl.t
  =
 fun untype_expression { binder; expr; attr } ->
  let ty = untype_type_expression expr.O.type_expression in
  let binder = Binder.map (Fn.const @@ Some ty) binder in
  let expr = untype_expression expr in
  let expr = I.e_ascription ~loc:expr.location expr ty in
  let attr = untype_value_attr attr in
  { binder; attr; expr }


and untype_declaration_pattern
    : (O.expression -> I.expression) -> _ O.Pattern_decl.t -> _ I.Pattern_decl.t
  =
 fun untype_expression { pattern; expr; attr } ->
  let ty = untype_type_expression expr.O.type_expression in
  let pattern = O.Pattern.map (Fn.const None) pattern in
  let expr = untype_expression expr in
  let expr = I.e_ascription ~loc:expr.location expr ty in
  let attr = untype_value_attr attr in
  { pattern; attr; expr }


and untype_declaration_type : _ O.Type_decl.t -> _ I.Type_decl.t =
 fun { type_binder; type_expr; type_attr = { public; hidden } } ->
  let type_expr = untype_type_expression type_expr in
  let type_attr = ({ public; hidden } : I.TypeOrModuleAttr.t) in
  { type_binder; type_expr; type_attr }


and untype_declaration_module : _ O.Module_decl.t -> _ I.Module_decl.t =
 fun { module_binder; module_; module_attr = { public; hidden }; annotation } ->
  let module_ = untype_module_expr module_ in
  let module_attr = ({ public; hidden } : I.TypeOrModuleAttr.t) in
  { module_binder; module_; module_attr; annotation }


and untype_declaration_signature
    : O.signature O.Signature_decl.t -> I.signature_expr I.Signature_decl.t
  =
 fun { signature_binder; signature; signature_attr } ->
  let signature = untype_signature signature in
  { signature_binder
  ; signature = Location.wrap ~loc:Location.generated (I.S_sig signature)
  ; signature_attr
  }


and untype_declaration =
  let return (d : I.declaration_content) = d in
  fun (d : O.declaration_content) ->
    match d with
    | D_value dc ->
      let dc = untype_declaration_constant untype_expression dc in
      return @@ D_value dc
    | D_irrefutable_match x ->
      let x = untype_declaration_pattern untype_expression x in
      return @@ D_irrefutable_match x
    | D_type dt ->
      let dt = untype_declaration_type dt in
      return @@ D_type dt
    | D_module_include module_ ->
      let module_ = untype_module_expr module_ in
      return @@ D_module_include module_
    | D_module dm ->
      let dm = untype_declaration_module dm in
      let dm = { dm with annotation = None } in
      return @@ D_module dm
    | D_signature ds ->
      let ds = untype_declaration_signature ds in
      return @@ D_signature ds


and untype_decl : O.decl -> I.decl = fun d -> Location.map untype_declaration d
and untype_module : O.module_ -> I.module_ = fun p -> List.map ~f:untype_decl p
