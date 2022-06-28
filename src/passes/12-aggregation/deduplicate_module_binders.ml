(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
 Break the export : do not use on libraries (preprocessor for ast_aggregated)
*)
module AST = Ast_typed

module Scope : sig
  type t
  val empty : t
  val new_module_var  : t -> AST.module_variable -> t  -> t * AST.module_variable
  val get_module_var  : t -> AST.module_variable -> t * AST.module_variable
end =
struct
  module MMap = Simple_utils.Map.Make(AST.ModuleVar)
  type t = {module_ : (AST.module_variable * t) MMap.t}
  let empty = {module_ = MMap.empty}
  let new_module_var map var mod_scope =
    let var' = match MMap.find_opt var map.module_ with
      Some (v,_) -> AST.ModuleVar.fresh_like ~loc:(AST.ModuleVar.get_location var) v
    | None -> var in
    let module_ = MMap.add var (var',mod_scope) map.module_ in
    {module_}, var'

  let get_module_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:(var,empty) @@ MMap.find_opt var map.module_
    |> fun (v,m) -> (m,AST.ModuleVar.(set_location @@ get_location var) v)

end

let rec type_expression : Scope.t -> AST.type_expression -> AST.type_expression = fun scope te ->
  let self ?(scope = scope) = type_expression scope in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    return @@ T_variable type_variable
  | T_sum {content;layout} ->
    let content = AST.LMap.map AST.(fun {associated_type;michelson_annotation;decl_pos} ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) content in
    return @@ T_sum {content;layout}
  | T_record {content;layout} ->
    let content = AST.LMap.map AST.(fun {associated_type;michelson_annotation;decl_pos} ->
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
  | T_abstraction {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_abstraction {ty_binder;kind;type_}
  | T_for_all {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}

let rec expression : Scope.t -> AST.expression -> AST.expression = fun scope e ->
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
    return @@ E_variable variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder={var;ascr;attributes};result} ->
    let ascr = Option.map ~f:self_type ascr in
    let result = self result in
    return @@ E_lambda {binder={var;ascr;attributes};result}
  | E_type_abstraction {type_binder;result} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,type_binder = Scope.new_type_var scope type_binder in *)
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}} ->
    let fun_type = self_type fun_type in
    let ascr = Option.map ~f:self_type ascr in
    let result = self result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder={var;ascr;attributes};result}}
  | E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr} ->
    let ascr = Option.map ~f:self_type ascr in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in {let_binder={var;ascr;attributes};rhs;let_result;attr}
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
    let record = AST.LMap.map self record in
    return @@ E_record record
  | E_record_accessor {record;path} ->
    let record = self record in
    return @@ E_record_accessor {record;path}
  | E_record_update {record;path;update} ->
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  | E_mod_in  {module_binder; rhs; let_result} ->
    let mod_scope,rhs = compile_module_expr scope rhs in
    let scope,module_binder = Scope.new_module_var scope module_binder mod_scope in
    let let_result = self ~scope let_result in
    return @@ E_mod_in {module_binder;rhs;let_result}
  | E_module_accessor {module_path;element} ->
    let _,module_path = List.fold_map ~init:(scope) module_path ~f:(Scope.get_module_var) in
    return @@ E_module_accessor {module_path;element}
  | E_assign {binder={var;ascr;attributes};access_path;expression} ->
    let ascr = Option.map ~f:self_type ascr in
    let expression = self expression in
    return @@ E_assign {binder={var;ascr;attributes};access_path;expression}

and matching_cases : Scope.t -> AST.matching_expr -> AST.matching_expr = fun scope me ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  let return x = x in
  match me with
    Match_variant {cases;tv} ->
    let cases = List.map ~f:AST.(fun {constructor;pattern;body} ->
        let body = self body in
        {constructor;pattern;body}
      ) cases in
    let tv   = self_type tv in
    return @@ AST.Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let fields = AST.LMap.map AST.(fun {var;ascr;attributes} ->
      let ascr = Option.map ~f:self_type ascr in
      {var;ascr;attributes}
    ) fields in
    let body = self body in
    let tv   = self_type tv in
    return @@ AST.Match_record {fields;body;tv}

and compile_declaration scope (d : AST.declaration) : Scope.t * AST.declaration =
  let return scope wrap_content = scope, {d with wrap_content} in
  match Location.unwrap d with
    Declaration_constant {binder;expr;attr} ->
      let expr   = expression scope expr in
      let binder = Stage_common.Maps.binder (type_expression scope) binder in
      return scope @@ AST.Declaration_constant {binder;expr;attr}
  | Declaration_type {type_binder;type_expr;type_attr} ->
      let type_expr = type_expression scope type_expr in
      return scope @@ AST.Declaration_type {type_binder;type_expr;type_attr}
  | Declaration_module {module_binder;module_;module_attr} ->
      let mod_scope,module_   = compile_module_expr scope module_ in
      let scope,module_binder = Scope.new_module_var scope module_binder mod_scope in
      return scope @@ AST.Declaration_module {module_binder;module_;module_attr}

and compile_declaration_list scope (program : AST.program) : Scope.t * AST.program =
  List.fold_map ~init:scope ~f:(compile_declaration) program

and compile_module_expr : Scope.t -> AST.module_expr -> Scope.t * AST.module_expr =
  fun scope mexpr ->
    let return scope wrap_content = scope,{mexpr with wrap_content} in
    match mexpr.wrap_content with
    | M_struct prg -> (
      let scope,prg = compile_declaration_list scope prg in
      return scope @@ AST.M_struct prg
    )
    | M_variable v -> (
      let scope,v = Scope.get_module_var scope v in
      return scope @@ AST.M_variable v
    )
    | M_module_path (hd,tl) -> (
      let scope,hd = Scope.get_module_var scope hd in
      let scope,tl = List.fold_map ~init:(scope) tl ~f:(Scope.get_module_var) in
      return scope @@ AST.M_module_path (hd,tl)
    )



let program : AST.program -> Scope.t * AST.program = fun prg ->
  let scope = Scope.empty in
  let scope,prg = compile_declaration_list scope prg in
  scope,prg

let expression ?(scope = Scope.empty) : AST.expression -> AST.expression = fun e ->
  let e = expression scope e in
  e


