open Trace
open Helpers

let compile (source_filename:string) syntax : Ast_simplified.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  ok simplified

let compile_string (source:string) syntax : Ast_simplified.program result =
  let%bind simplified = parsify_string syntax source in
  ok simplified

let compile_expression : v_syntax -> string -> Ast_simplified.expression result =
    fun syntax exp ->
  parsify_expression syntax exp

let compile_contract_input : string -> string -> v_syntax -> Ast_simplified.expression result =
    fun storage parameter syntax ->
  let%bind (storage,parameter) = bind_map_pair (compile_expression syntax) (storage,parameter) in
  ok @@ Ast_simplified.e_pair storage parameter
