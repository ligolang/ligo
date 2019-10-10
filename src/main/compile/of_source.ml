open Trace
open Helpers

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

let compile_expression_as_function : string -> s_syntax -> _ result =
  fun expression syntax ->
  let%bind syntax = syntax_to_variant syntax None in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression_as_function simplified

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    syntax (source_filename:string) : Ast_typed.program result =
  let _ = debug_simplify, debug_typed, syntax, source_filename in
  failwith "TODO"
  (* let%bind syntax = syntax_to_variant syntax (Some source_filename) in
   * let%bind simpl = parsify syntax source_filename in
   * (if debug_simplify then
   *    Format.(printf "Simplified : %a\n%!" Ast_simplified.PP.program simpl)
   * ) ;
   * let%bind typed =
   *   trace (simple_error "typing") @@
   *   Typer.type_program simpl in
   * (if debug_typed then (
   *     Format.(printf "Typed : %a\n%!" Ast_typed.PP.program typed)
   *   )) ;
   * ok typed *)
