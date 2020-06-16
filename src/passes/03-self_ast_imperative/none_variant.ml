open Errors
open Ast_imperative
open Trace

let peephole_expression : expression -> (expression , self_ast_imperative_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_constructor {constructor=Constructor "Some";element=e} -> return @@ E_constant {cons_name=C_SOME;arguments=[ e ]}
  | E_constructor {constructor=Constructor "None"; _} -> return @@ E_constant {cons_name=C_NONE ; arguments=[]}
  | E_matching {matchee;cases=Match_variant [((Constructor "None", _),none_expr);((Constructor "Some", some),some_expr)]}
  | E_matching {matchee;cases=Match_variant [((Constructor "Some", some),some_expr);((Constructor "None", _),none_expr)]}
  ->
    let match_none = none_expr in
    let match_some = some,some_expr in
    let cases = Match_option {match_none;match_some} in 
    return @@ E_matching {matchee;cases}
  | e -> return e
