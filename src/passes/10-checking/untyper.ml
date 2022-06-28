module Ligo_string = Simple_utils.Ligo_string
module Location    = Simple_utils.Location
module I = Ast_core
module O = Ast_typed

let untype_literal (l:O.literal) : I.literal =
  let open I in
  match l with
  | Literal_unit -> Literal_unit
  | Literal_nat n -> (Literal_nat n)
  | Literal_timestamp n -> (Literal_timestamp n)
  | Literal_mutez n -> (Literal_mutez n)
  | Literal_int n -> (Literal_int n)
  | Literal_string s -> (Literal_string s)
  | Literal_signature s -> (Literal_signature s)
  | Literal_key s -> (Literal_key s)

  | Literal_key_hash s -> (Literal_key_hash s)
  | Literal_chain_id s -> (Literal_chain_id s)
  | Literal_bytes b -> (Literal_bytes b)
  | Literal_address s -> (Literal_address s)
  | Literal_operation s -> (Literal_operation s)
  | Literal_bls12_381_g1 b -> (Literal_bls12_381_g1 b)
  | Literal_bls12_381_g2 b -> (Literal_bls12_381_g2 b)
  | Literal_bls12_381_fr b -> (Literal_bls12_381_fr b)
  | Literal_chest b -> Literal_chest b
  | Literal_chest_key b -> Literal_chest_key b

let rec untype_type_expression (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
      let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
        let associated_type = untype_type_expression associated_type in
        let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
        v' in
      let x' = I.LMap.map aux content in
      return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = I.LMap.map aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable name
  | O.T_arrow arr ->
    let arr = Stage_common.Maps.arrow self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let arguments = List.map ~f:self parameters in
    let type_operator = I.TypeVar.fresh ~name:(Stage_common.Constant.to_string injection) () in
    return @@ I.T_app {type_operator;arguments}
  | O.T_singleton l ->
    return @@ I.T_singleton l
  | O.T_abstraction x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_for_all {x with type_}

let rec untype_expression (e:O.expression) : I.expression =
  untype_expression_content e.type_expression e.expression_content
and untype_expression_content ty (ec:O.expression_content) : I.expression =
  let open I in
  let return e = e in
  match ec with
  | E_literal l ->
      let l = untype_literal l in
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:untype_expression arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      return (e_variable (n))
  | E_application {lamb;args} ->
      let f' = untype_expression lamb in
      let arg' = untype_expression args in
      return (e_application f' arg')
  | E_lambda {binder ; result} -> (
      let _, ty = Ast_typed.Helpers.destruct_for_alls ty in
      let { type1 ; type2 } = O.get_t_arrow_exn ty in
      let (input_type , output_type) =
        Simple_utils.Pair.map ~f:untype_type_expression (type1, type2) in
      let result = untype_expression result in
      return (e_lambda {binder with ascr=Some input_type} (Some output_type) result)
    )
  | E_type_abstraction {type_binder;result} -> (
    let result = untype_expression result in
    return (e_type_abs type_binder result)
  )
  | E_constructor {constructor; element} ->
      let p' = untype_expression element in
      return (e_constructor constructor p')
  | E_record r ->
    let r' = LMap.map untype_expression r in
    return (e_record r' ())
  | E_record_accessor {record; path} ->
      let r' = untype_expression record in
      let Label s = path in
      return (e_record_accessor r' (Label s))
  | E_record_update {record=r; path=Label l; update=e} ->
    let r' = untype_expression r in
    let e = untype_expression e in
    return (e_record_update r' (I.Label l) e)
  | E_matching {matchee;cases} -> (
    let matchee = untype_expression matchee in
    match cases with
    | Match_variant {cases ; tv} ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : Ast_typed.matching_content_case -> _ match_case =
        fun { constructor ; pattern ; body } -> (
          let pattern =
            match tv with
            | _ ->
              let proj = Location.wrap @@ P_var { ascr = None ; var = pattern ; attributes = Stage_common.Helpers.empty_attribute } in
              Location.wrap @@ P_variant (constructor, proj)
          in
          let body = untype_expression body in
          ({pattern ; body } : (Ast_core.expression, Ast_core.type_expression) match_case)
        )
      in
      let cases = List.map ~f:aux cases in
      return (e_matching matchee cases)
    | Match_record {fields ; body ; tv=_} -> (
      let aux : (Ast_typed.label * Ast_typed.type_expression binder) -> label * Ast_core.type_expression pattern =
        fun (Ast_typed.Label label, binder) -> (
          let proj = Location.wrap @@ P_var {binder with ascr = Option.map ~f:untype_type_expression binder.ascr} in
          (Label label, proj)
        )
      in
      let (labels,patterns) = List.unzip @@ List.map ~f:aux (LMap.to_kv_list fields) in
      let body = untype_expression body in
      let case = match Ast_typed.Helpers.is_tuple_lmap fields with
        | false ->
          let pattern = Location.wrap (P_record (labels,patterns)) in
          ({ pattern ; body } : _ Ast_core.match_case)
        | true ->
          let pattern = Location.wrap (P_tuple patterns) in
          ({ pattern ; body } : _ Ast_core.match_case)
      in
      return (e_matching matchee [case])
    )
  )
  | E_let_in {let_binder;rhs;let_result; attr} ->
      let tv = untype_type_expression rhs.type_expression in
      let rhs = untype_expression rhs in
      let result = untype_expression let_result in
      return (e_let_in {let_binder with ascr=(Some tv)} rhs result attr)
  | E_mod_in {module_binder;rhs;let_result} ->
      let rhs = untype_module_expr rhs in
      let result = untype_expression let_result in
      return @@ e_mod_in module_binder rhs result
  | E_raw_code {language; code} ->
      let code = untype_expression code in
      return (e_raw_code language code)
  | E_recursive {fun_name;fun_type; lambda} ->
      let fun_type = untype_type_expression fun_type in
      let unty_expr= untype_expression_content ty @@ E_lambda lambda in
      let lambda = match unty_expr.expression_content with I.E_lambda l -> l | _ -> failwith "impossible case" in
      return @@ e_recursive fun_name fun_type lambda
  | E_module_accessor ma -> return @@ I.make_e @@ E_module_accessor ma
  | E_assign a ->
    let a = Stage_common.Maps.assign untype_expression untype_type_expression a in
    return @@ make_e @@ E_assign a
  | E_type_inst {forall;type_=type_inst} ->
    match forall.type_expression.type_content with
    | T_for_all {ty_binder;type_;kind=_} ->
      let type_ = Ast_typed.Helpers.subst_type ty_binder type_inst type_ in
      let forall = { forall with type_expression = type_ } in
      untype_expression forall
    | T_arrow _ ->
      (* This case is used for external typers *)
      untype_expression forall
    | _ ->
      failwith "Impossible case: cannot untype a type instance of a non polymorphic type"

and untype_module_expr : O.module_expr -> I.module_expr =
  fun module_expr ->
    let return wrap_content = { module_expr with wrap_content } in
    match module_expr.wrap_content with
    | M_struct prg ->
      let prg = untype_program prg in
      return (I.M_struct prg)
    | M_module_path path ->
      return (I.M_module_path path)
    | M_variable v ->
      return (I.M_variable v)
and untype_declaration_constant : (O.expression -> I.expression) -> O.declaration_constant -> I.declaration_constant =
  fun untype_expression O.{binder;expr;attr} ->
    let ty = untype_type_expression expr.type_expression in
    let var = binder.var in
    let binder = ({var;ascr=Some ty;attributes=Stage_common.Helpers.empty_attribute}: _ I.binder) in
    let expr = untype_expression expr in
    let expr = I.e_ascription expr ty in
    I.{binder;attr;expr;}

and untype_declaration_type : O.declaration_type -> I.declaration_type =
  fun O.{type_binder; type_expr; type_attr={public;hidden}} ->
    let type_expr = untype_type_expression type_expr in
    let type_attr = (I.{public;hidden}: I.type_attribute) in
    I.{type_binder; type_expr; type_attr}

and untype_declaration_module : O.declaration_module -> I.declaration_module =
  fun O.{module_binder; module_; module_attr={public;hidden}} ->
    let module_ = untype_module_expr module_ in
    let module_attr = (I.{public;hidden}: I.module_attribute) in
    I.{module_binder; module_ ; module_attr}

and untype_declaration =
  let return (d: I.declaration_content) = d in
  fun (d: O.declaration_content) -> match d with
  | Declaration_constant dc ->
    let dc = untype_declaration_constant untype_expression dc in
    return @@ Declaration_constant dc
  | Declaration_type dt ->
    let dt = untype_declaration_type dt in
    return @@ Declaration_type dt
  | Declaration_module dm ->
    let dm = untype_declaration_module dm in
    return @@ Declaration_module dm
and untype_declarations : O.module_ -> I.module_ = fun p ->
  List.map ~f:(Location.map untype_declaration) p

and untype_program : O.module_ -> I.module_ = fun x -> untype_declarations x
