open Trace
open Helpers
open Tezos_utils

let parse_file_program source_filename syntax =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  ok simplified

let compile_file_entry : string -> string -> s_syntax -> _ result =
  fun source_filename entry_point syntax ->
  let%bind simplified = parse_file_program source_filename syntax in
  Of_simplified.compile_function_entry simplified entry_point

let compile_file_contract_entry : string -> string -> s_syntax -> _ result =
  fun source_filename entry_point syntax ->
  let%bind simplified = parse_file_program source_filename syntax in
  let%bind compiled_contract = Of_simplified.compile_contract_entry simplified entry_point in
  ok compiled_contract

let compile_file_contract_parameter : string -> string -> string -> s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression simplified

let compile_file_expression : string -> string -> string -> s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression simplified

let compile_file_contract_storage : string -> string -> string -> s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression simplified

let compile_file_contract_args =
  fun source_filename _entry_point storage parameter syntax ->
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind storage_simplified = parsify_expression syntax storage in
  let%bind parameter_simplified = parsify_expression syntax parameter in
  let args = Ast_simplified.e_pair storage_simplified parameter_simplified in
  Of_simplified.compile_expression args

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    syntax (source_filename:string) : Ast_typed.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simpl = parsify syntax source_filename in
  (if debug_simplify then
     Format.(printf "Simplified : %a\n%!" Ast_simplified.PP.program simpl)
  ) ;
  let%bind typed =
    trace (simple_error "typing") @@
    Typer.type_program simpl in
  (if debug_typed then (
      Format.(printf "Typed : %a\n%!" Ast_typed.PP.program typed)
    )) ;
  ok typed
