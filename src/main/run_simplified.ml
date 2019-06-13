open Trace

let run_simplityped
    ?options
    ?(debug_mini_c = false) ?(debug_michelson = false)
    (program : Ast_typed.program) (entry : string)
    (input : Ast_simplified.expression) : Ast_simplified.expression result =
  let%bind typed_input =
    let env =
      let last_declaration = Location.unwrap List.(hd @@ rev program) in
      match last_declaration with
      | Declaration_constant (_ , (_ , post_env)) -> post_env
    in
    Typer.type_expression env input in  
  let%bind typed_result =
    Run_typed.run_typed ?options ~debug_mini_c ~debug_michelson entry program typed_input in
  let%bind annotated_result = Typer.untype_expression typed_result in
  ok annotated_result

let evaluate_simplityped ?options (program : Ast_typed.program) (entry : string)
  : Ast_simplified.expression result =
  let%bind typed_result = Run_typed.evaluate_typed ?options entry program in
  let%bind annotated_result = Typer.untype_expression typed_result in
  ok annotated_result
