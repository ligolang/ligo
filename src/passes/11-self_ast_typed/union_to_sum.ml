open Ligo_prim
open Simple_utils.Trace
module Self_ast_typed_helpers = Helpers
open Ast_typed

let reduce_union_to_sum union =
  let fields =
    union
    |> Union.Injection.injections_of_union
    |> List.map ~f:(fun injection ->
           let label = Label.of_string (Union.Injection.constructor_name injection) in
           let summand = Union.Injection.source injection in
           label, summand)
  in
  let layout = fields |> List.map ~f:fst |> Layout.default_of_labels in
  let row = Row.of_alist_exn ~layout fields in
  row


let reduce_type_expression ty_expr =
  match ty_expr.type_content with
  | T_union union ->
    if Union.number_of_summands union >= 1
    then (
      let type_content = T_sum (reduce_union_to_sum union) in
      { ty_expr with type_content })
    else t_never ~loc:ty_expr.location ()
  | _ -> ty_expr


let reduce_union_match
    (match_ : (Ast_typed.expression, Ast_typed.type_expression) Union.Match.t)
  =
  let matchee = Union.Match.matchee match_ in
  let cases =
    let branches = Union.Match.branches match_ in
    branches
    |> List.map ~f:(fun union_branch ->
           let pattern =
             let Binder.{ var; ascr = summand } =
               union_branch |> Union.Match.Branch.pattern |> Union.Match.Pattern.to_binder
             in
             let constructor =
               union_branch
               |> Union.Match.Branch.pattern
               |> Union.Match.Pattern.injection
               |> Union.Injection.constructor_name
               |> Label.of_string
             in
             let var_pattern =
               Pattern.P_var (Binder.make var summand)
               |> Location.wrap ~loc:Location.generated
             in
             Pattern.P_variant (constructor, var_pattern)
             |> Location.wrap ~loc:Location.generated
           in
           let body = Union.Match.Branch.body union_branch in
           let case : (expression, type_expression) Match_expr.match_case =
             { pattern; body }
           in
           case)
  in
  E_matching { matchee; cases }


let rec reduce_expression expr =
  match expr.expression_content with
  | E_union_injected injected ->
    let injection = Union.Injected.injection injected in
    let expression_content =
      let expr_in_summand = Union.Injected.expr_in_source injected in
      let constructor = Label.of_string (Union.Injection.constructor_name injection) in
      E_constructor { constructor; element = expr_in_summand }
    in
    let type_expression =
      let union = Union.Injection.target injection in
      let type_content = T_sum (reduce_union_to_sum union) in
      { expr.type_expression with type_content }
    in
    { expr with expression_content; type_expression }
  | E_union_match match_ -> { expr with expression_content = reduce_union_match match_ }
  | E_union_use use -> reduce_expression (Union.Use.after_expansion use)
  | _ -> expr


let program
    :  raise:(Errors.self_ast_typed_error, _) raise -> Ast_typed.program
    -> Ast_typed.program
  =
 fun ~raise prg ->
  prg
  |> Self_ast_typed_helpers.map_program reduce_expression
  |> Self_ast_typed_helpers.Type_mapper.map_program reduce_type_expression


let expression
    :  raise:(Errors.self_ast_typed_error, _) raise -> Ast_typed.expression
    -> Ast_typed.expression
  =
 fun ~raise expr ->
  expr
  |> Self_ast_typed_helpers.map_expression reduce_expression
  |> Self_ast_typed_helpers.Type_mapper.map_expression reduce_type_expression
