(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
 Break the export : do not use on libraries (preprocessor for ast_aggregated)
*)
open Ligo_prim
module AST = Ast_typed

module Scope : sig
  type t
  val empty : t
  val new_module_var  : t -> Module_var.t -> t  -> t * Module_var.t
  val get_module_var  : t -> Module_var.t -> t * Module_var.t
end =
struct
  module MMap = Simple_utils.Map.Make(Module_var)
  type t = {module_ : (Module_var.t * t) MMap.t}
  let empty = {module_ = MMap.empty}
  let new_module_var map var mod_scope =
    let var' = match MMap.find_opt var map.module_ with
      Some (v,_) -> Module_var.fresh_like ~loc:(Module_var.get_location var) v
    | None -> var in
    let module_ = MMap.add var (var',mod_scope) map.module_ in
    {module_}, var'

  let get_module_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:(var,empty) @@ MMap.find_opt var map.module_
    |> fun (v,m) -> (m,Module_var.(set_location @@ get_location var) v)

end

let rec type_expression : Scope.t -> AST.type_expression -> AST.type_expression = fun scope te ->
  let self ?(scope = scope) = type_expression scope in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable type_variable ->
    return @@ T_variable type_variable
  | T_sum {fields;layout} ->
    let fields = Record.map ~f:(fun ({associated_type;michelson_annotation;decl_pos}:AST.row_element) : AST.row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_sum {fields;layout}
  | T_record {fields;layout} ->
    let fields = Record.map ~f:(fun ({associated_type;michelson_annotation;decl_pos}:AST.row_element) : AST.row_element ->
      let associated_type = self associated_type in
      {associated_type;michelson_annotation;decl_pos}
    ) fields in
    return @@ T_record {fields;layout}
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
  | E_lambda l ->
    let l = Lambda.map self self_type l in
    return @@ E_lambda l
  | E_type_abstraction {type_binder;result} ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,type_binder = Scope.new_type_var scope type_binder in *)
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive r ->
    let r = Recursive.map self self_type r in
    return @@ E_recursive r
  | E_let_in {let_binder;rhs;let_result;attributes} ->
    let let_binder = AST.Pattern.map self_type let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in {let_binder;rhs;let_result;attributes}
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
    let record = Record.map ~f:self record in
    return @@ E_record record
  | E_accessor {struct_;path} ->
    let struct_ = self struct_ in
    return @@ E_accessor {struct_;path}
  | E_update {struct_;path;update} ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update {struct_;path;update}
  | E_mod_in  {module_binder; rhs; let_result} ->
    let mod_scope,rhs = compile_module_expr scope rhs in
    let scope,module_binder = Scope.new_module_var scope module_binder mod_scope in
    let let_result = self ~scope let_result in
    return @@ E_mod_in {module_binder;rhs;let_result}
  | E_module_accessor {module_path;element} ->
    let _,module_path = List.fold_map ~init:(scope) module_path ~f:(Scope.get_module_var) in
    return @@ E_module_accessor {module_path;element}
  | E_let_mut_in { let_binder ; rhs ; let_result ; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    let let_binder = AST.Pattern.map self_type let_binder in
    return (E_let_mut_in { let_binder ; rhs ; let_result ; attributes })
  | E_deref var -> return (E_deref var)
  | E_assign {binder;expression} ->
    let binder = Binder.map self_type binder in
    let expression = self expression in
    return @@ E_assign {binder;expression}
  | E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ E_for for_loop
  | E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ E_for_each for_each_loop
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop

and matching_cases 
: Scope.t -> (AST.expression, AST.type_expression) AST.Match_expr.match_case list 
  -> (AST.expression, AST.type_expression) AST.Match_expr.match_case list 
= fun scope me ->
  let self ?(scope = scope) = expression scope in
  let self_type ?(scope = scope) = type_expression scope in
  List.map me ~f:(AST.Match_expr.map_match_case self self_type)

and compile_declaration scope (d : AST.declaration) : Scope.t * AST.declaration =
  let return scope wrap_content = scope, {d with wrap_content} in
  match Location.unwrap d with
    D_value {binder;expr;attr} ->
      let expr   = expression scope expr in
      return scope @@ AST.D_value {binder;expr;attr}
  | D_irrefutable_match  {pattern;expr;attr} ->
      let expr    = expression scope expr in
      let pattern = AST.Pattern.map (type_expression scope) pattern in
      return scope @@ AST.D_irrefutable_match  {pattern;expr;attr}
  | D_type {type_binder;type_expr;type_attr} ->
      let type_expr = type_expression scope type_expr in
      return scope @@ AST.D_type {type_binder;type_expr;type_attr}
  | D_module {module_binder;module_;module_attr} ->
      let mod_scope,module_   = compile_module_expr scope module_ in
      let scope,module_binder = Scope.new_module_var scope module_binder mod_scope in
      return scope @@ AST.D_module {module_binder;module_;module_attr}

and compile_program scope (program : AST.program) : Scope.t * AST.program =
  List.fold_map ~init:scope ~f:(compile_declaration) program

and compile_decl : Scope.t -> AST.decl -> Scope.t * AST.decl =
  fun s d -> compile_declaration s d


and compile_module scope (m : AST.module_) : Scope.t * AST.module_ =
  List.fold_map ~init:scope ~f:(compile_decl) m

and compile_module_expr : Scope.t -> AST.module_expr -> Scope.t * AST.module_expr =
  fun scope mexpr ->
    let return scope wrap_content: _ * AST.module_expr = scope,{mexpr with wrap_content} in
    match mexpr.wrap_content with
    | M_struct prg -> (
      let scope,prg = compile_module scope prg in
      return scope @@ M_struct prg
    )
    | M_variable v -> (
      let scope,v = Scope.get_module_var scope v in
      return scope @@ M_variable v
    )
    | M_module_path (hd,tl) -> (
      let scope,hd = Scope.get_module_var scope hd in
      let scope,tl = List.fold_map ~init:(scope) tl ~f:(Scope.get_module_var) in
      return scope @@ M_module_path (hd,tl)
    )



let program : AST.program -> Scope.t * AST.program = fun prg ->
  let scope = Scope.empty in
  let scope,prg = compile_program scope prg in
  scope,prg

let expression ?(scope = Scope.empty) : AST.expression -> AST.expression = fun e ->
  let e = expression scope e in
  e


