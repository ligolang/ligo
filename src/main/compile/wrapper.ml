open Trace

let source_to_typed syntax source_file =
  let%bind simplified  = Of_source.compile source_file syntax in
  let%bind typed,state = Of_simplified.compile simplified in
  let env = Ast_typed.program_environment typed in
  ok (typed,state,env)

let source_to_typed_expression ~env ~state parameter syntax =
  let%bind simplified = Of_source.compile_expression syntax parameter in
  let%bind (typed,_) = Of_simplified.compile_expression ~env ~state simplified in
  ok typed
