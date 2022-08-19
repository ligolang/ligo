module Ligo_string = Simple_utils.Ligo_string
module Location    = Simple_utils.Location
module I = Ast_core
module O = Ast_typed
open Ligo_prim

let untype_value_attr : O.Attr.value -> I.Attr.value =
  fun {inline;no_mutation;view;public;hidden} -> {inline;no_mutation;view;public;hidden}
let rec untype_type_expression (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {fields ; layout} ->
      let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
        let associated_type = self associated_type in
        let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
        v' in
      let x' = Record.map aux fields in
      return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {fields;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = self associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = Record.map aux fields in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable name
  | O.T_arrow arr ->
    let arr = Arrow.map self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let arguments = List.map ~f:self parameters in
    let type_operator = TypeVar.fresh ~name:(Literal_types.to_string injection) () in
    return @@ I.T_app {type_operator;arguments}
  | O.T_singleton l ->
    return @@ I.T_singleton l
  | O.T_abstraction x ->
    let x = Abstraction.map self x in
    return @@ T_abstraction x
  | O.T_for_all x ->
    let x = Abstraction.map self x in
    return @@ T_for_all x

let untype_type_expression_option = fun x -> Option.return @@ untype_type_expression x

let rec untype_expression (e:O.expression) : I.expression =
  untype_expression_content e.expression_content
and untype_expression_content (ec:O.expression_content) : I.expression =
  let open I in
  let self = untype_expression in
  let self_type = untype_type_expression in
  let self_type_opt = untype_type_expression_option in
  let return e = e in
  match ec with
  | E_literal l ->
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:self arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      return (e_variable (n))
  | E_application {lamb;args} ->
      let f' = self lamb in
      let arg' = self args in
      return (e_application f' arg')
  | E_lambda {binder ; output_type; result} -> (
      let binder = Binder.map self_type_opt binder in
      let output_type = self_type_opt output_type in
      let result = self result in
      return (e_lambda binder output_type result)
    )
  | E_type_abstraction {type_binder;result} -> (
    let result = self result in
    return (e_type_abs type_binder result)
  )
  | E_constructor {constructor; element} ->
      let p' = self element in
      return (e_constructor constructor p')
  | E_record r ->
    let r' = Record.map self r in
    return (e_record r' ())
  | E_accessor {record; path} ->
      let r' = self record in
      let Label s = path in
      return (e_record_accessor r' (Label s))
  | E_update {record=r; path=Label l; update=e} ->
    let r' = self r in
    let e = self e in
    return (e_record_update r' (Label l) e)
  | E_matching {matchee;cases} -> (
    let matchee = self matchee in
    match cases with
    | Match_variant {cases ; tv} ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : _ Ast_typed.matching_content_case -> _ Match_expr.match_case =
        fun { constructor ; pattern ; body } -> (
          let pattern =
            match tv with
            | _ ->
              let proj = Location.wrap @@ Pattern.P_var { ascr = None ; var = pattern ; attributes = Binder.empty_attribute } in
              Location.wrap @@ Pattern.P_variant (constructor, proj)
          in
          let body = self body in
          ({pattern ; body } : (Ast_core.expression, Ast_core.type_expression option) Match_expr.match_case)
        )
      in
      let cases = List.map ~f:aux cases in
      return (e_matching matchee cases)
    | Match_record {fields ; body ; tv=_} -> (
      let aux : (Label.t * Ast_typed.type_expression Binder.t) -> Label.t * Ast_core.type_expression option Pattern.t =
        fun (Label label, binder) -> (
          let proj = Location.wrap @@ Pattern.P_var {binder with ascr = Option.return @@ self_type binder.Binder.ascr} in
          (Label label, proj)
        )
      in
      let (labels,patterns) = List.unzip @@ List.map ~f:aux (Record.LMap.to_kv_list fields) in
      let body = self body in
      let case = match Record.is_tuple fields with
        | false ->
          let pattern = Location.wrap (Pattern.P_record (labels,patterns)) in
          ({ pattern ; body } : _ Match_expr.match_case)
        | true ->
          let pattern = Location.wrap (Pattern.P_tuple patterns) in
          ({ pattern ; body } : _ Match_expr.match_case)
      in
      return (e_matching matchee [case])
    )
  )
  | E_let_in {let_binder;rhs;let_result; attr} ->
      let tv = self_type rhs.type_expression in
      let rhs = self rhs in
      let result = self let_result in
      let attr : Attr.value = untype_value_attr attr in
      return (e_let_in {let_binder with ascr=(Some tv)} rhs result attr)
  | E_mod_in {module_binder;rhs;let_result} ->
      let rhs = untype_module_expr rhs in
      let result = self let_result in
      return @@ e_mod_in module_binder rhs result
  | E_raw_code {language; code} ->
      let code = self code in
      return (e_raw_code language code)
  | E_recursive {fun_name;fun_type; lambda} ->
      let fun_type = self_type fun_type in
      let lambda = Lambda.map self self_type lambda in
      return @@ e_recursive fun_name fun_type lambda
  | E_module_accessor ma -> return @@ I.make_e @@ E_module_accessor ma
  | E_assign a ->
    let a = Assign.map self self_type_opt a in
    return @@ make_e @@ E_assign a
  | E_type_inst {forall;type_=type_inst} ->
    match forall.type_expression.type_content with
    | T_for_all {ty_binder;type_;kind=_} ->
      let type_ = Ast_typed.Helpers.subst_type ty_binder type_inst type_ in
      let forall = { forall with type_expression = type_ } in
      self forall
    | T_arrow _ ->
      (* This case is used for external typers *)
      self forall
    | _ ->
      failwith "Impossible case: cannot untype a type instance of a non polymorphic type"

and untype_module_expr : O.module_expr -> I.module_expr =
  fun module_expr ->
    let return wrap_content : I.module_expr = { module_expr with wrap_content } in
    match module_expr.wrap_content with
    | M_struct prg ->
      let prg = untype_module prg in
      return (M_struct prg)
    | M_module_path path ->
      return (M_module_path path)
    | M_variable v ->
      return (M_variable v)
and untype_declaration_constant : (O.expression -> I.expression) -> _ O.Declaration.declaration_constant -> _ I.Declaration.declaration_constant =
  fun untype_expression {binder;expr;attr} ->
    let ty = untype_type_expression expr.O.type_expression in
    let var = binder.var in
    let binder = ({var;ascr=Some ty;attributes=Binder.empty_attribute}: _ Binder.t) in
    let expr = untype_expression expr in
    let expr = I.e_ascription expr ty in
    let attr = untype_value_attr attr in
    {binder;attr;expr;}

and untype_declaration_type : _ O.Declaration.declaration_type -> _ I.Declaration.declaration_type =
  fun {type_binder; type_expr; type_attr={public;hidden}} ->
    let type_expr = untype_type_expression type_expr in
    let type_attr = ({public;hidden}: I.Attr.type_) in
    {type_binder; type_expr; type_attr}

and untype_declaration_module : _ O.Declaration.declaration_module -> _ I.Declaration.declaration_module =
  fun {module_binder; module_; module_attr={public;hidden}} ->
    let module_ = untype_module_expr module_ in
    let module_attr = ({public;hidden}: I.Attr.module_) in
    {module_binder; module_ ; module_attr}

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

and untype_decl : O.decl -> I.decl =
  fun (Decl d) -> Decl (Location.map untype_declaration d)

and untype_module : O.module_ -> I.module_ = fun p ->
  List.map ~f:(untype_decl) p

and untype_program : O.program -> I.program = fun p ->
  List.map ~f:(Location.map untype_declaration) p
