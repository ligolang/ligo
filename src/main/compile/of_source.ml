open Trace
open Helpers
open Tezos_utils

let parse_file_program source_filename syntax =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  ok simplified

let compile_file_entry : string -> string -> s_syntax -> Compiler.Program.compiled_program result =
  fun source_filename entry_point syntax ->
  let%bind simplified = parse_file_program source_filename syntax in
  Of_simplified.compile_function_entry simplified entry_point

let compile_file_parameter : string -> string -> string -> s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression simplified
