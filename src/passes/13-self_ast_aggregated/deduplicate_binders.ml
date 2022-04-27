(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
 The algorithm works in two step :
  1. Replace all variable by adding a "'" at the end. If the variable was already seen, add an extra "'"
  2. Replace the variables with the most "'" by it's base name (for each similar base name).
2. is done in order to preserve the exported names for the files
TODO : actually take into consideration the export
*)
open Ast_aggregated

module Scope : sig
  type t
  val empty : t
  val new_value_var  : t -> expression_variable -> t * expression_variable
  val get_value_var  : t -> expression_variable -> expression_variable
  val new_type_var   : t -> type_variable -> t * type_variable
  val get_type_var   : t -> type_variable -> type_variable
end =
struct
  module VMap = Simple_utils.Map.Make(ValueVar)
  module TMap = Simple_utils.Map.Make(TypeVar)
  type t = {value:expression_variable VMap.t;type_:type_variable TMap.t}
  let empty = {value = VMap.empty;type_ = TMap.empty}
  let new_value_var map var =
    let var' = match VMap.find_opt var map.value with
      Some (v) -> ValueVar.fresh_like ~loc:(ValueVar.get_location var) v
    | None -> ValueVar.fresh_like var in
    let value = VMap.add var var' map.value in
    {map with value}, var'

  let get_value_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ VMap.find_opt var map.value
    |> ValueVar.set_location  @@ ValueVar.get_location var

  let new_type_var (map : t) var =
    let var' = match TMap.find_opt var map.type_ with
      Some (v) -> TypeVar.fresh_like ~loc:(TypeVar.get_location var) v
    | None -> TypeVar.fresh_like var in
    let type_ = TMap.add var var' map.type_ in
    {map with type_}, var'

  let get_type_var (map : t) var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ TMap.find_opt var map.type_
    |> TypeVar.set_location @@ TypeVar.get_location var

end

let rec type_expression : Scope.t -> type_expression -> type_expression = fun scope te ->
  let self ?(scope = scope) = type_expression scope in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    let type_variable = Scope.get_type_var scope type_variable in
    return @@ T_variable type_variable
  | T_sum {content;layout} ->
    let content = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) content in
    return @@ T_sum {content;layout}
  | T_record {content;layout} ->
    let content = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) content in
    return @@ T_record {content;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_constant {language;injection;parameters} ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant {language;injection;parameters}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_for_all {ty_binder;kind;type_} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,ty_binder = Scope.new_type_var scope ty_binder in *)
    let type_ = self ~scope type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let rec expression : Scope.t -> expression -> expression = fun scope e ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return expression_content = {e with expression_content} in
  match e.expression_content with
    E_literal literal ->
    return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant {cons_name;arguments}
  | E_variable variable ->
    let variable = Scope.get_value_var scope variable in
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder={var;ascr;attributes};result} ->
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let result = self ~scope result in
    return @@ E_lambda {binder={var;ascr;attributes};result}
  | E_type_abstraction {type_binder;result} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,type_binder = Scope.new_type_var scope type_binder in *)
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}} ->
    let fun_name = Scope.get_value_var scope fun_name in
    let fun_type = self_type fun_type in
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let result = self ~scope result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}}
  | E_let_in {let_binder={var;ascr;attributes};rhs={expression_content=E_recursive _;} as rhs;let_result;attr} ->
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let rhs = self ~scope rhs in
    let let_result = self ~scope let_result in
    return @@ E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr}
  | E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr} ->
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let rhs = self rhs in
    let let_result = self ~scope let_result in
    return @@ E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr}
  | E_type_in {type_binder;rhs;let_result} ->
    let scope,type_binder = Scope.new_type_var scope type_binder in
    let rhs = self_type rhs in
    let let_result = self ~scope let_result in
    return @@ E_type_in {type_binder;rhs;let_result}
  | E_type_inst {forall; type_} ->
    let forall = self forall in
    let type_  = self_type type_ in
    return @@ E_type_inst {forall; type_}
  | E_raw_code {language;code} ->
    let code = self code in
    return @@ E_raw_code {language;code}
  | E_constructor {constructor; element} ->
    let element = self element in
    return @@ E_constructor {constructor; element}
  | E_matching {matchee;cases} ->
    let matchee = self matchee in
    let cases = matching_cases scope cases in
    return @@ E_matching {matchee;cases}
  | E_record record ->
    let record = LMap.map self record in
    return @@ E_record record
  | E_record_accessor {record;path} ->
    let record = self record in
    return @@ E_record_accessor {record;path}
  | E_record_update {record;path;update} ->
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  | E_assign {binder={var;ascr;attributes};access_path;expression} ->
    let var = Scope.get_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let expression = self expression in
    return @@ E_assign {binder={var;ascr;attributes};access_path;expression}

and matching_cases : Scope.t -> matching_expr -> matching_expr = fun scope me ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return x = x in
  match me with
    Match_variant {cases;tv} ->
    let cases = List.map ~f:(fun {constructor;pattern;body} ->
        let scope,pattern = Scope.new_value_var scope pattern in
        let body = self ~scope body in
        {constructor;pattern;body}
      ) cases in
    let tv   = self_type tv in
    return @@ Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let scope,fields = LMap.fold_map ~f:(fun _ {var;ascr;attributes} scope ->
      let scope,var = Scope.new_value_var scope var in
      let ascr = Option.map ~f:self_type ascr in
      scope,{var;ascr;attributes}
    ) ~init:scope fields in
    let body = self ~scope body in
    let tv   = self_type tv in
    return @@ Match_record {fields;body;tv}


let expression : expression -> expression = fun e ->
  let scope = Scope.empty in
  expression scope e
