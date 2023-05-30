open Ligo_prim
module AST = Ast_typed

(* Aliases records module aliasing (Module M = A.B.C.D),
in order to remove then and replace M.toto with A.B.C.D.toto.

Module_var.t list is the path A.B.C.D in reverse order and
the inside t is the aliases inside A.B.C.D in order to
correctly recode and resolve nested parttern of modules
and aliasing

*)
module Aliases = struct
  module MMap = Simple_utils.Map.Make (Module_var)

  type t = { inside : (t * Module_var.t list) MMap.t }

  let empty = { inside = MMap.empty }

  let push aliases mvar mod_aliases path =
    { inside = MMap.add mvar (mod_aliases, path) aliases.inside }


  let get aliases mvar = MMap.find mvar aliases.inside

  let diff_path path module_path =
    let path = List.rev path in
    let module_path = List.rev module_path in
    let rec aux path module_path =
      match path, module_path with
      | hd1 :: tl1, hd2 :: tl2 when Module_var.equal hd1 hd2 -> aux tl1 tl2
      | _ -> path
    in
    List.rev @@ aux path module_path
end

(* this is doing nothing ? *)
let rec type_expression : Aliases.t -> AST.type_expression -> AST.type_expression =
 fun aliases te ->
  let self ?(aliases = aliases) = type_expression aliases in
  let return type_content = { te with type_content } in
  match te.type_content with
  | T_variable type_variable -> return @@ T_variable type_variable
  | T_sum { fields; layout } ->
    let fields = Record.map ~f:self fields in
    return @@ T_sum { fields; layout }
  | T_record { fields; layout } ->
    let fields = Record.map ~f:self fields in
    return @@ T_record { fields; layout }
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2 }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant { language; injection; parameters }
  | T_singleton literal -> return @@ T_singleton literal
  | T_abstraction { ty_binder; kind; type_ } ->
    let type_ = self type_ in
    return @@ T_abstraction { ty_binder; kind; type_ }
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = self type_ in
    return @@ T_for_all { ty_binder; kind; type_ }


let rec expression path : Aliases.t -> AST.expression -> AST.expression =
 fun aliases e ->
  let self ?(aliases = aliases) = expression path aliases in
  let self_type ?(aliases = aliases) = type_expression aliases in
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_literal literal -> return @@ E_literal literal
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant { cons_name; arguments }
  | E_variable variable -> return @@ E_variable variable
  | E_application { lamb; args } ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application { lamb; args }
  | E_lambda l ->
    let l = Lambda.map self self_type l in
    return @@ E_lambda l
  | E_type_abstraction { type_binder; result } ->
    let result = self result in
    return @@ E_type_abstraction { type_binder; result }
  | E_recursive r ->
    let r = Recursive.map self self_type r in
    return @@ E_recursive r
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let let_binder = AST.Pattern.map self_type let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    let type_ = self_type type_ in
    return @@ E_type_inst { forall; type_ }
  | E_raw_code { language; code } ->
    let code = self code in
    return @@ E_raw_code { language; code }
  | E_constructor { constructor; element } ->
    let element = self element in
    return @@ E_constructor { constructor; element }
  | E_matching { matchee; cases } ->
    let matchee = self matchee in
    let cases = matching_cases path aliases cases in
    return @@ E_matching { matchee; cases }
  | E_record record ->
    let record = Record.map ~f:self record in
    return @@ E_record record
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update { struct_; path; update }
  | E_mod_in { module_binder; rhs; let_result } ->
    let mod_aliases, path, rhs = compile_module_expr module_binder [] aliases rhs in
    let aliases = Aliases.push aliases module_binder mod_aliases path in
    let let_result = self ~aliases let_result in
    (match rhs with
    | None -> let_result
    | Some rhs -> return @@ E_mod_in { module_binder; rhs; let_result })
  | E_module_accessor { module_path; element } ->
    let _, module_path =
      List.fold ~init:(aliases, path) module_path ~f:(fun (a, _module_path) mvar ->
          let aliases, path' = Aliases.get a mvar in
          let path = Aliases.diff_path path' path in
          aliases, path)
    in
    let module_path = List.rev module_path in
    return @@ E_module_accessor { module_path; element }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    let let_binder = AST.Pattern.map self_type let_binder in
    return (E_let_mut_in { let_binder; rhs; let_result; attributes })
  | E_deref var -> return (E_deref var)
  | E_assign { binder; expression } ->
    let binder = Binder.map self_type binder in
    let expression = self expression in
    return @@ E_assign { binder; expression }
  | E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ E_for for_loop
  | E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ E_for_each for_each_loop
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop


and matching_cases path
    :  Aliases.t -> (AST.expression, AST.type_expression) AST.Match_expr.match_case list
    -> (AST.expression, AST.type_expression) AST.Match_expr.match_case list
  =
 fun scope me ->
  let self ?(scope = scope) = expression path scope in
  let self_type ?(scope = scope) = type_expression scope in
  List.map me ~f:(AST.Match_expr.map_match_case self self_type)


and compile_declaration path aliases (d : AST.declaration)
    : Aliases.t * AST.declaration option
  =
  let return_s aliases wrap_content = aliases, Some { d with wrap_content } in
  let return_n aliases = aliases, None in
  match Location.unwrap d with
  | D_value { binder; expr; attr } ->
    let expr = expression path aliases expr in
    let binder = Binder.map (type_expression aliases) binder in
    return_s aliases @@ AST.D_value { binder; expr; attr }
  | D_irrefutable_match { pattern; expr; attr } ->
    let expr = expression path aliases expr in
    let pattern = AST.Pattern.map (type_expression aliases) pattern in
    return_s aliases @@ AST.D_irrefutable_match { pattern; expr; attr }
  | D_type { type_binder; type_expr; type_attr } ->
    let type_expr = type_expression aliases type_expr in
    return_s aliases @@ AST.D_type { type_binder; type_expr; type_attr }
  | D_module { module_binder; module_; module_attr; annotation = () } ->
    let mod_aliases, path, module_ =
      compile_module_expr module_binder path aliases module_
    in
    let aliases = Aliases.push aliases module_binder mod_aliases path in
    (match module_ with
    | None -> return_n aliases
    | Some module_ ->
      return_s aliases @@ AST.D_module { module_binder; module_; module_attr; annotation = () })


and compile_declaration_list path aliases (program : AST.program)
    : Aliases.t * AST.program
  =
  let aliases, dcl = List.fold_map ~init:aliases ~f:(compile_declaration path) program in
  let dcl = List.filter_opt dcl in
  aliases, dcl


and compile_decl path : Aliases.t -> AST.decl -> Aliases.t * AST.decl option =
 fun s d -> compile_declaration path s d


and compile_module path aliases (m : AST.module_) : Aliases.t * AST.module_ =
  let aliases, dcl = List.fold_map ~init:aliases ~f:(compile_decl path) m in
  let dcl = List.filter_opt dcl in
  aliases, dcl


and compile_module_expr mvar path
    :  Aliases.t -> AST.module_expr
    -> Aliases.t * Module_var.t list * AST.module_expr option
  =
 fun aliases mexpr ->
  match mexpr.module_content with
  | M_struct prg ->
    let aliases, prg = compile_module (mvar :: path) aliases prg in
    aliases, mvar :: path, Some { mexpr with module_content = M_struct prg }
  | M_variable v ->
    let aliases, path' = Aliases.get aliases v in
    aliases, path', None
  | M_module_path (hd, tl) ->
    let aliases, module_path =
      List.fold ~init:(aliases, path) (hd :: tl) ~f:(fun (a, _module_path) mvar ->
          let aliases, path' = Aliases.get a mvar in
          aliases, path')
    in
    aliases, module_path, None


let program : AST.program -> Aliases.t * AST.program =
 fun prg ->
  let aliases = Aliases.empty in
  compile_declaration_list [] aliases prg


let expression ?(aliases = Aliases.empty) : AST.expression -> AST.expression =
 fun e ->
  let e = expression [] aliases e in
  e
