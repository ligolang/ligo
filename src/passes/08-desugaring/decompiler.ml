module I = Ast_sugar
module O = Ast_core

module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair
open Ligo_prim

let decompile_exp_attributes : O.ValueAttr.t -> I.Attr.t = fun { inline ; no_mutation ; view ; public ; hidden ; thunk } ->
  let aux : string list -> (unit -> string option) -> I.Attr.t = fun acc is_fun ->
    match is_fun () with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold ~init:[] ~f:aux
    [
      (fun () -> if inline then Some "inline" else None) ;
      (fun () -> if no_mutation then Some "no_mutation" else None) ;
      (fun () -> if view then Some "view" else None) ;
      (fun () -> if public then None else Some "private") ;
      (fun () -> if hidden then Some "hidden" else None) ;
      (fun () -> if thunk then Some "thunk" else None) ;
    ]

let decompile_type_attributes : O.TypeOrModuleAttr.t -> I.Attr.t = fun { public ; hidden } ->
  let aux : string list -> (unit -> string option) -> I.Attr.t = fun acc is_fun ->
    match is_fun () with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold ~init:[] ~f:aux
    [
      (fun () -> if public then None else Some "private") ;
      (fun () -> if hidden then Some "hidden" else None) ;
    ]
let decompile_module_attributes = decompile_type_attributes

let rec decompile_type_expression : O.type_expression -> I.type_expression =
  fun te ->
  let self = decompile_type_expression in
  let return te = I.make_t te in
  match te.sugar with
    Some te -> te
  | None ->
    match te.type_content with
      | O.T_variable type_variable -> return @@ T_variable type_variable
      | O.T_app tc ->
        let tc = Type_app.map self tc in
        return @@ T_app tc
      | O.T_sum {fields;layout} ->
        let fields =
          Record.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ Rows.row_element = {associated_type;attributes;decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" Layout.pp l))] | None -> [] in
        return @@ I.T_sum {fields ; attributes}
      | O.T_record {fields;layout} ->
        let fields =
          Record.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ Rows.row_element = {associated_type ; attributes ; decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" Layout.pp l))] | None -> [] in
        return @@ I.T_record { fields ; attributes }
      | O.T_arrow arr ->
        let arr = Arrow.map self arr in
        return @@ T_arrow arr
      | O.T_module_accessor ma -> return @@ T_module_accessor ma
      | O.T_singleton x -> return @@ I.T_singleton x
      | O.T_abstraction x ->
        let type_ = self x.type_ in
        return @@ I.T_abstraction { x with type_ }
      | O.T_for_all x ->
        let type_ = self x.type_ in
        return @@ I.T_for_all { x with type_ }

let decompile_type_expression_option = Option.map ~f:decompile_type_expression
let decompile_pattern_to_string pattern =
  let p = O.Pattern.map (decompile_type_expression_option) pattern in
  Purification.Decompiler.decompile_pattern_to_string p

let rec decompile_expression : O.expression -> I.expression =
  fun e ->
  let self = decompile_expression in
  let self_type = decompile_type_expression in
  let self_type_opt = decompile_type_expression_option in
  let return expr = I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> e
  | None ->
    match e.expression_content with
      O.E_literal lit -> return @@ I.E_literal (lit)
    | O.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:self arguments in
      return @@ I.E_constant {cons_name = cons_name;arguments}
    | O.E_variable name -> return @@ I.E_variable name
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
    | O.E_let_in {let_binder;attr={inline=false;no_mutation=_;view=_;public=_;hidden=_;thunk=false};rhs=expr1;let_result=expr2}
      when Value_var.is_name (Binder.get_var let_binder) "()"
           && Stdlib.(=) (Binder.get_ascr let_binder) (Some (O.t_unit ())) ->
      let expr1 = self expr1 in
      let expr2 = self expr2 in
      return @@ I.E_sequence {expr1;expr2}
    | O.E_let_in {let_binder;attr;rhs;let_result} ->
      let let_binder = Binder.map self_type_opt let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attributes = if attr.inline then ["inline"] else [] in
      return @@ I.E_let_in {let_binder;attributes;rhs;let_result}
    | O.E_type_in {type_binder; rhs; let_result} ->
      let rhs = self_type rhs in
      let let_result = self let_result in
      return @@ I.E_type_in {type_binder; rhs; let_result}
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
      let m = I.Match_expr.map self self_type_opt m in
      return @@ I.E_matching m
    | O.E_record recd ->
      let recd = Record.map self recd in
      return @@ I.E_record recd
    | O.E_accessor {struct_;path} ->
      let struct_ = self struct_ in
      let Label path  = path in
      return @@ I.E_accessor {struct_;path=[Access_record path]}
    | O.E_update {struct_;path;update} ->
      let struct_ = self struct_ in
      let update = self update in
      let Label path  = path in
      return @@ I.E_update {struct_;path=[Access_record path];update}
    | O.E_ascription {anno_expr; type_annotation} ->
      let anno_expr = self anno_expr in
      let type_annotation = decompile_type_expression type_annotation in
      return @@ I.E_ascription {anno_expr; type_annotation}
    | O.E_module_accessor ma -> return @@ E_module_accessor ma
    | O.E_assign a ->
      let a = Assign.map self self_type_opt a in
      return @@ I.E_assign a
    | O.E_for for_loop ->
      let for_loop = For_loop.map self for_loop in
      return @@ I.E_for for_loop
    | O.E_for_each for_each_loop ->
      let for_each_loop = For_each_loop.map self for_each_loop in
      return @@ I.E_for_each for_each_loop
    | O.E_while while_loop ->
      let while_loop = While_loop.map self while_loop in
      return @@ I.E_while while_loop
    | O.E_let_mut_in { let_binder; rhs; let_result; attr } ->
      let let_binder = Binder.map self_type_opt let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attributes = decompile_exp_attributes attr in
      return @@ I.E_let_mut_in { let_binder; attributes; rhs; let_result }

and decompile_declaration : O.declaration -> I.declaration = fun d ->
  let return wrap_content : I.declaration = {d with wrap_content} in
  match Location.unwrap d with
  | D_value {binder;expr;attr} ->
    let binder = Binder.map decompile_type_expression_option binder in
    let expr   = decompile_expression expr in
    let attr   = decompile_exp_attributes attr in
    return @@ D_value {binder;expr;attr}
  | D_type {type_binder;type_expr;type_attr} ->
    let type_expr = decompile_type_expression type_expr in
    let type_attr = decompile_type_attributes type_attr in
    return @@ D_type {type_binder;type_expr;type_attr}
  | D_module {module_binder;module_;module_attr} ->
    let module_ = decompile_module_expr module_ in
    let module_attr = decompile_module_attributes module_attr in
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
