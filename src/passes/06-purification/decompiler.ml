
module Pair = Simple_utils.Pair
module Location = Simple_utils.Location
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Ligo_prim

module Helpers = Ast_sugar.Helpers

let rec decompile_type_expression : O.type_expression -> I.type_expression =
  fun te ->
  let self = decompile_type_expression in
  let return te = I.make_t te in
  match te.type_content with
    | O.T_variable type_variable -> return @@ T_variable type_variable
    | O.T_app tc ->
      let tc = Type_app.map self tc in
      return @@ I.T_app tc
    | O.T_sum {fields ; attributes} ->
      (* This type sum could be a michelson_or as well, we could use is_michelson_or *)
      let fields = List.map ~f:(fun (k,v) -> (k,Rows.map_row_element self v)) (O.LMap.to_kv_list fields) in
      return @@ I.T_sum { fields ; attributes }
    | O.T_record {fields ; attributes} ->
      let fields = List.map ~f:(fun (k,v) -> (k,Rows.map_row_element self v)) (O.LMap.to_kv_list fields) in
      return @@ I.T_record { fields ; attributes }
    | O.T_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ I.T_tuple tuple
    | O.T_arrow arr ->
      let arr = Arrow.map self arr in
      return @@ T_arrow arr
    | O.T_module_accessor ma -> return @@ I.T_module_accessor ma
    | O.T_singleton x ->
      return @@ I.T_singleton x
    | O.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ I.T_abstraction {x with type_}
    | O.T_for_all x ->
      let type_ = self x.type_ in
      return @@ I.T_for_all {x with type_}

let decompile_type_expression_option = Option.map ~f:decompile_type_expression

let decompile_pattern_to_string ~syntax pattern =
  let pattern = Helpers.Conv.record_to_list pattern in
  let p = I.Pattern.map (decompile_type_expression_option) pattern in
  let s = match syntax with
    Some Syntax_types.JsLIGO ->
      Tree_abstraction.Jsligo.decompile_pattern_to_string p
  | Some CameLIGO ->
      Tree_abstraction.Cameligo.decompile_pattern_to_string p
  | Some ReasonLIGO ->
      Tree_abstraction.Reasonligo.decompile_pattern_to_string p
  | Some PascaLIGO ->
      Tree_abstraction.Pascaligo.decompile_pattern_to_string p
  | None ->
      Tree_abstraction.Cameligo.decompile_pattern_to_string p
  in s

let rec decompile_expression : O.expression -> I.expression =
  fun e ->
  let self      = decompile_expression in
  let self_type = decompile_type_expression in
  let self_type_opt = decompile_type_expression_option in
  let return expr = I.make_e ~loc:e.location expr in
  match e.expression_content with
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_constant {cons_name;arguments} ->
    let cons_name = Constant.Const cons_name in
    let arguments = List.map ~f:decompile_expression arguments in
    return @@ I.E_constant {cons_name;arguments}
  | O.E_application app ->
    let app = Application.map self app in
    return @@ I.E_application app
  | O.E_lambda lamb ->
    let lamb = Lambda.map self self_type_opt lamb in
    return @@ I.E_lambda lamb
  | O.E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return @@ I.E_type_abstraction ta
  | O.E_recursive recs ->
    let recs = Recursive.map self self_type recs in
    return @@ I.E_recursive recs
  | O.E_let_in {let_binder;attributes;rhs;let_result} ->
    let let_binder = Binder.map (Option.map ~f:decompile_type_expression) let_binder in
    let rhs = decompile_expression rhs in
    let let_result = decompile_expression let_result in
    return @@ I.E_let_in {let_binder;attributes;rhs;let_result}
  | O.E_type_in ti ->
    let ti = Type_in.map self self_type ti in
    return @@ I.E_type_in ti
  | O.E_mod_in {module_binder;rhs;let_result} ->
    let rhs = decompile_module_expr rhs in
    let let_result = self let_result in
    return @@ I.E_mod_in {module_binder;rhs;let_result}
  | O.E_raw_code rc ->
    let rc = Raw_code.map self rc in
    return @@ I.E_raw_code rc
  | O.E_constructor const ->
    let const = Constructor.map self const in
    return @@ I.E_constructor const
  | O.E_matching m ->
    let O.Match_expr.{matchee;cases} = O.Match_expr.map self self_type_opt m in
    let cases = List.map cases ~f:(fun {pattern;body} -> 
      let pattern = Helpers.Conv.record_to_list pattern in
      I.Match_expr.{pattern;body}) in
    return @@ I.E_matching {matchee;cases}
  | O.E_record recd ->
    let recd = Record.map self recd in
    return @@ I.E_record (Record.LMap.to_kv_list recd)
  | O.E_accessor acc ->
    let acc = I.Accessor.map self acc in
    return @@ I.E_accessor acc
  | O.E_update up ->
    let up = I.Update.map self up in
    return @@ I.E_update up
  | O.E_tuple tuple ->
    let tuple = List.map ~f:self tuple in
    return @@ I.E_tuple tuple
  | O.E_ascription ascr ->
    let ascr = Ascription.map self self_type ascr in
    return @@ I.E_ascription ascr
  | O.E_module_accessor ma -> return @@ I.E_module_accessor ma
  | O.E_cond cond ->
    let cond = Conditional.map self cond in
    return @@ I.E_cond cond
  | O.E_sequence seq ->
    let seq = Sequence.map self seq in
    return @@ I.E_sequence seq
  | O.E_skip () -> return @@ I.E_skip ()
  | O.E_map map ->
    let map = List.map ~f:(
      Pair.map ~f:self
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let big_map = List.map ~f:(
      Pair.map ~f:self
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let lst = List.map ~f:self lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let set = List.map ~f:self set in
    return @@ I.E_set set
  | O.E_assign a ->
    let a = Assign.map self self_type_opt a in
    return @@ I.E_assign a
  | O.E_for { binder; start; final; incr; f_body } ->
    let start = self start in
    let final = self final in
    let incr = self incr in
    let f_body = self f_body in
    return @@ I.E_for { binder; start; final; incr; f_body }
  | O.E_for_each { fe_binder; collection; collection_type; fe_body } ->
    let collection = self collection in
    let fe_body = self fe_body in
    return @@ I.E_for_each { fe_binder; collection; collection_type; fe_body }
  | O.E_while { cond; body } ->
    let cond = self cond in
    let body = self body in
    return @@ I.E_while { cond; body }
  | O.E_let_mut_in { let_binder; attributes; rhs; let_result } ->
    let let_binder = Binder.map (Option.map ~f:decompile_type_expression) let_binder in
    let rhs = decompile_expression rhs in
    let let_result = decompile_expression let_result in
    return @@ I.E_let_mut_in { let_binder; attributes; rhs; let_result }

and decompile_declaration : O.declaration -> I.declaration = fun d ->
  let return wrap_content : I.declaration = {d with wrap_content} in
  match Location.unwrap d with
  | D_value {binder;expr;attr} ->
    let binder = Binder.map decompile_type_expression_option binder in
    let expr   = decompile_expression expr in
    return @@ D_value {binder;expr;attr}
  | D_type {type_binder;type_expr;type_attr} ->
    let type_expr = decompile_type_expression type_expr in
    return @@ D_type {type_binder;type_expr;type_attr}
  | D_module {module_binder;module_;module_attr} ->
    let module_ = decompile_module_expr module_ in
    return @@ D_module {module_binder;module_;module_attr}

and decompile_module_expr : O.module_expr -> I.module_expr = fun me ->
  let return wrap_content : I.module_expr = {me with wrap_content} in
  match me.wrap_content with
    M_struct lst ->
      let lst = decompile_module lst in
      return @@ M_struct lst
  | M_variable mv ->
      return @@ M_variable mv
  | M_module_path mp ->
      return @@ M_module_path mp

and decompile_decl : O.decl -> I.decl = fun d -> decompile_declaration d
and decompile_module : O.module_ -> I.module_ = fun m ->
  List.map ~f:decompile_decl m

let decompile_program = List.map ~f:decompile_declaration
