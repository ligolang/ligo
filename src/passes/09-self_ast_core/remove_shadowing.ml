(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
 The algorithm works in two step :
  1. Replace all variable by adding a "'" at the end. If the variable was already seen, add an extra "'"
  2. Replace the variables with the most "'" by it's base name (for each similar base name).
2. is done in order to preserve the exported names for the files
TODO : actually take into consideration the export
*)
open Ast_core

module Scope : sig
  type t
  val empty : t
  val new_value_var  : t -> expression_variable -> t * expression_variable
  val get_value_var  : t -> expression_variable -> expression_variable
  val new_type_var   : t -> type_variable -> t * type_variable
  val get_type_var   : t -> type_variable -> type_variable
  val new_module_var : t -> module_variable -> t * module_variable
  val get_module_var : t -> module_variable -> module_variable
  type swapper = {
    value   : expression_variable -> expression_variable;
    type_   : type_variable       -> type_variable;
    module_ : module_variable     -> module_variable;
  }
  val make_swapper : t -> swapper
end =
struct
  module VMap = Simple_utils.Map.Make(ValueVar)
  module TMap = Simple_utils.Map.Make(TypeVar)
  module MMap = Simple_utils.Map.Make(ModuleVar)
  type t = {value:expression_variable VMap.t;type_:type_variable TMap.t;module_:module_variable MMap.t}
  let empty = {value = VMap.empty;type_ = TMap.empty;module_ = MMap.empty}
  let new_value_var map var =
    let var' = match VMap.find_opt var map.value with
      Some (var) -> ValueVar.fresh_like var
    | None -> ValueVar.fresh_like var in
    let value = VMap.add var var' map.value in
    {map with value}, var'

  let get_value_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ VMap.find_opt var map.value

  let new_type_var map var =
    let var' = match TMap.find_opt var map.type_ with
      Some (var) -> TypeVar.fresh_like var
    | None -> TypeVar.fresh_like var in
    let type_ = TMap.add var var' map.type_ in
    {map with type_}, var'

  let get_type_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ TMap.find_opt var map.type_

    let new_module_var map var =
    let var' = match MMap.find_opt var map.module_ with
      Some (var) -> ModuleVar.fresh_like var
    | None -> ModuleVar.fresh_like var in
    let module_ = MMap.add var var' map.module_ in
    {map with module_}, var'

  let get_module_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ MMap.find_opt var map.module_

  type swapper = {
    value   : expression_variable -> expression_variable;
    type_   : type_variable       -> type_variable;
    module_ : module_variable     -> module_variable;
  }
  let make_swapper (scope:t) =
    let swap_value   = List.map ~f:(fun (k,v) -> v,k) in
    let value   = VMap.of_list @@ swap_value @@ VMap.to_kv_list scope.value in
    let type_   = TMap.of_list @@ swap_value @@ TMap.to_kv_list scope.type_ in
    let module_ = MMap.of_list @@ swap_value @@ MMap.to_kv_list scope.module_ in
    {
      value   = (fun map v -> Option.value ~default:v @@ VMap.find_opt v map) value ;
      type_   = (fun map v -> Option.value ~default:v @@ TMap.find_opt v map) type_;
      module_ = (fun map v -> Option.value ~default:v @@ MMap.find_opt v map) module_
    }
end

let rec swap_type_expression : Scope.swapper -> type_expression -> type_expression = fun swaper te ->
  let self = swap_type_expression swaper in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    let type_variable = swaper.type_ type_variable in
    return @@ T_variable type_variable
  | T_sum {fields;layout} ->
    let fields = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_sum {fields;layout}
  | T_record {fields;layout} ->
    let fields = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_record {fields;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_app {type_operator;arguments} ->
    let type_operator = swaper.type_ type_operator in
    let arguments = List.map ~f:self arguments in
    return @@ T_app {type_operator;arguments}
  | T_module_accessor {module_name;element} ->
    let module_name = swaper.module_ module_name in
    let element = self element in
    return @@ T_module_accessor {module_name;element}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_abstraction {ty_binder;kind;type_} ->
    let ty_binder = swaper.type_ ty_binder in
    let type_ = self type_ in
    return @@ T_abstraction {ty_binder;kind;type_}
  | T_for_all {ty_binder;kind;type_} ->
    let ty_binder = swaper.type_ ty_binder in
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let rec swap_expression : Scope.swapper -> expression -> expression = fun swaper e ->
  let self = swap_expression swaper in
  let self_type = swap_type_expression swaper in
  let return expression_content = {e with expression_content} in
  match e.expression_content with
    E_literal literal ->
    return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant {cons_name;arguments}
  | E_variable variable ->
    let variable = swaper.value variable in
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder={var;ascr;attributes};output_type;result} ->
    let var = swaper.value var in
    let ascr = Option.map ~f:self_type ascr in
    let output_type = Option.map ~f:self_type output_type in
    let result = self result in
    return @@ E_lambda {binder={var;ascr;attributes};output_type;result}
  | E_type_abstraction {type_binder;result} ->
    let type_binder = swaper.type_ type_binder in
    let result = self result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};output_type;result}} ->
    let fun_name = swaper.value fun_name in
    let fun_type = self_type fun_type in
    let var = swaper.value var in
    let ascr = Option.map ~f:self_type ascr in
    let output_type = Option.map ~f:self_type output_type in
    let result = self result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};output_type;result}}
  | E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr} ->
    let var = swaper.value var in
    let ascr = Option.map ~f:self_type ascr in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr}
  | E_type_in {type_binder;rhs;let_result} ->
    let type_binder = swaper.type_ type_binder in
    let rhs = self_type rhs in
    let let_result = self let_result in
    return @@ E_type_in {type_binder;rhs;let_result}
  | E_mod_in {module_binder;rhs;let_result} ->
    let rhs = swap_variable_in_module_ swaper rhs in
    let module_binder = swaper.module_ module_binder in
    let let_result = self let_result in
    return @@ E_mod_in {module_binder;rhs;let_result}
  | E_mod_alias {alias;binders;result} ->
    let binders = let (hd,tl) = binders in let hd = swaper.module_ hd in (hd,tl) in
    let alias = swaper.module_ alias in
    let result = self result in
    return @@ E_mod_alias {alias;binders;result}
  | E_raw_code {language;code} ->
    let code = self code in
    return @@ E_raw_code {language;code}
  | E_constructor {constructor; element} ->
    let element = self element in
    return @@ E_constructor {constructor; element}
  | E_matching {matchee;cases} ->
    let matchee = self matchee in
    let cases = matching_cases swaper cases in
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
  | E_ascription {anno_expr;type_annotation} ->
    let anno_expr = self anno_expr in
    let type_annotation = self_type type_annotation in
    return @@ E_ascription {anno_expr;type_annotation}
  | E_module_accessor {module_name; element} ->
    let module_name = swaper.module_ module_name in
    let element = self element in
    return @@ E_module_accessor {module_name; element}

and matching_cases : Scope.swapper -> _ match_case list -> _ match_case list = fun swaper mc_list ->
  let self = swap_expression swaper in
  let self_type = swap_type_expression swaper in
  let rec pat pattern =
    let return p = Location.wrap ~loc:(Location.get_location pattern) p in
    match Location.unwrap pattern with
      P_unit -> return @@ P_unit
    | P_var {var;ascr;attributes} ->
      let var = swaper.value var in
      let ascr = Option.map ~f:self_type ascr in
      return @@ P_var {var;ascr;attributes}
    | P_list (Cons (p,p')) ->
      let p = pat p in
      let p' = pat p' in
      return @@ P_list (Cons (p,p'))
    | P_list (List p_list) ->
      let p_list = List.map ~f:pat p_list in
      return @@ P_list (List p_list)
    | P_variant (label,pattern) ->
      let pattern = pat pattern in
      return @@ P_variant (label,pattern)
    | P_tuple p_list ->
      let p_list = List.map ~f:pat p_list in
      return @@ P_tuple p_list
    | P_record (l_list,p_list) ->
      let p_list = List.map ~f:pat p_list in
      return @@ P_record (l_list,p_list)
  in
  List.map mc_list ~f:(fun {pattern;body} ->
    let pattern = pat pattern in
    let body = self body in
    {pattern;body})

and declaration : Scope.swapper -> declaration Location.wrap -> declaration Location.wrap = fun swaper d ->
  let loc = d.location in
  let self = swap_expression swaper in
  let self_type = swap_type_expression swaper in
  let return d = Location.wrap ~loc d  in
  match Location.unwrap d with
    Declaration_type {type_binder;type_expr;type_attr} ->
    let type_expr = self_type type_expr in
    let type_binder = swaper.type_ type_binder in
    return @@ Declaration_type {type_binder;type_expr;type_attr}
  | Declaration_constant {binder={var;ascr;attributes};attr;expr} ->
    let expr = self expr in
    let var = swaper.value var in
    let ascr = Option.map ~f:(self_type) ascr in
    return @@ Declaration_constant {binder={var;ascr;attributes};attr;expr}
  | Declaration_module {module_binder;module_=m;module_attr} ->
    let m = swap_variable_in_module_ swaper m in
    let module_binder = swaper.module_ module_binder in
    return @@ Declaration_module {module_binder;module_=m;module_attr}
  | Module_alias {alias;binders} ->
    let binders = let (hd,tl) = binders in let hd = swaper.module_ hd in (hd,tl) in
    let alias = swaper.module_ alias in
    return @@ Module_alias {alias;binders}
and swap_variable_in_module_ : Scope.swapper -> module_ -> module_ = fun swaper m ->
  (* this breaks the exported value -> solution swap the first occurence with the last, *)
  let module_ = List.map m ~f:(declaration swaper) in
  module_

let rec type_expression : Scope.t -> type_expression -> type_expression = fun scope te ->
  let self ?(scope = scope) = type_expression scope in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    let type_variable = Scope.get_type_var scope type_variable in
    return @@ T_variable type_variable
  | T_sum {fields;layout} ->
    let fields = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_sum {fields;layout}
  | T_record {fields;layout} ->
    let fields = LMap.map (fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_record {fields;layout}
  | T_arrow {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_app {type_operator;arguments} ->
    let type_operator = Scope.get_type_var scope type_operator in
    let arguments = List.map ~f:self arguments in
    return @@ T_app {type_operator;arguments}
  | T_module_accessor {module_name;element} ->
    let module_name = Scope.get_module_var scope module_name in
    let element = self element in
    return @@ T_module_accessor {module_name;element}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_abstraction {ty_binder;kind;type_} ->
    let scope,ty_binder = Scope.new_type_var scope ty_binder in
    let type_ = self ~scope type_ in
    return @@ T_abstraction {ty_binder;kind;type_}
  | T_for_all {ty_binder;kind;type_} ->
    let scope,ty_binder = Scope.new_type_var scope ty_binder in
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
  | E_lambda {binder={var;ascr;attributes};output_type;result} ->
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let output_type = Option.map ~f:self_type output_type in
    let result = self ~scope result in
    return @@ E_lambda {binder={var;ascr;attributes};output_type;result}
  | E_type_abstraction {type_binder;result} ->
    let scope,type_binder = Scope.new_type_var scope type_binder in
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};output_type;result}} ->
    let scope,fun_name = Scope.new_value_var scope fun_name in
    let fun_type = self_type fun_type in
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:self_type ascr in
    let output_type = Option.map ~f:self_type output_type in
    let result = self ~scope result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};output_type;result}}
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
  | E_mod_in {module_binder;rhs;let_result} ->
    let rhs = module_ scope rhs in
    let scope,module_binder = Scope.new_module_var scope module_binder in
    let let_result = self ~scope let_result in
    return @@ E_mod_in {module_binder;rhs;let_result}
  | E_mod_alias {alias;binders;result} ->
    let binders = let (hd,tl) = binders in let hd = Scope.get_module_var scope hd in (hd,tl) in
    let scope,alias = Scope.new_module_var scope alias in
    let result = self ~scope result in
    return @@ E_mod_alias {alias;binders;result}
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
  | E_ascription {anno_expr;type_annotation} ->
    let anno_expr = self anno_expr in
    let type_annotation = self_type type_annotation in
    return @@ E_ascription {anno_expr;type_annotation}
  | E_module_accessor {module_name; element} ->
    let module_name = Scope.get_module_var scope module_name in
    let element = self element in
    return @@ E_module_accessor {module_name; element}

and matching_cases : Scope.t -> _ match_case list -> _ match_case list = fun scope mc_list ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let rec pat scope pattern =
    let return scope p = scope,Location.wrap ~loc:(Location.get_location pattern) p in
    match Location.unwrap pattern with
      P_unit -> return scope @@ P_unit
    | P_var {var;ascr;attributes} ->
      let scope,var = Scope.new_value_var scope var in
      let ascr = Option.map ~f:self_type ascr in
      return scope @@ P_var {var;ascr;attributes}
    | P_list (Cons (p,p')) ->
      let scope,p = pat scope p in
      let scope,p' = pat scope p' in
      return scope @@ P_list (Cons (p,p'))
    | P_list (List p_list) ->
      let scope,p_list = List.fold_map ~f:pat ~init:scope p_list in
      return scope @@ P_list (List p_list)
    | P_variant (label,pattern) ->
      let scope,pattern = pat scope pattern in
      return scope @@ P_variant (label,pattern)
    | P_tuple p_list ->
      let scope,p_list = List.fold_map ~f:pat ~init:scope p_list in
      return scope @@ P_tuple p_list
    | P_record (l_list,p_list) ->
      let scope,p_list = List.fold_map ~f:pat ~init:scope p_list in
      return scope @@ P_record (l_list,p_list)
  in
  List.map mc_list ~f:(fun {pattern;body} ->
    let scope,pattern = pat scope pattern in
    let body = self ~scope body in
    {pattern;body})

and declaration : Scope.t -> declaration Location.wrap -> Scope.t * declaration Location.wrap = fun scope d ->
  let loc = d.location in
  let return scope d = scope, Location.wrap ~loc d  in
  match Location.unwrap d with
    Declaration_type {type_binder;type_expr;type_attr} ->
    let type_expr = type_expression scope type_expr in
    let scope,type_binder = Scope.new_type_var scope type_binder in
    return scope @@ Declaration_type {type_binder;type_expr;type_attr}
  | Declaration_constant {binder={var;ascr;attributes};attr;expr} ->
    let expr = expression scope expr in
    let scope,var = Scope.new_value_var scope var in
    let ascr = Option.map ~f:(type_expression scope) ascr in
    return scope @@ Declaration_constant {binder={var;ascr;attributes};attr;expr}
  | Declaration_module {module_binder;module_=m;module_attr} ->
    let m = module_ scope m in
    let scope,module_binder = Scope.new_module_var scope module_binder in
    return scope @@ Declaration_module {module_binder;module_=m;module_attr}
  | Module_alias {alias;binders} ->
    let binders = let (hd,tl) = binders in let hd = Scope.get_module_var scope hd in (hd,tl) in
    let scope,alias = Scope.new_module_var scope alias in
    return scope @@ Module_alias {alias;binders}

and module_ : Scope.t -> module_ -> module_ = fun scope m ->
  (* this breaks the exported value -> solution swap the first occurence with the last, *)
  let scope,module_ = List.fold_map m ~init:scope ~f:(declaration) in
  let swaper  = Scope.make_swapper scope in
  let module_ = swap_variable_in_module_ swaper module_ in
  module_

let program : program -> program = fun p ->
  let scope = Scope.empty in
  module_ scope p

let expression : expression -> expression = fun e ->
  let scope = Scope.empty in
  expression scope e
