open Trace
open Helpers

type c_unit = Buffer.t

let compile ?(libs=[]) c_unit (source_filename:string) syntax : (Ast_imperative.program , _) result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  parse_and_abstract ~libs syntax c_unit source_filename

let compile_expression : ?libs: string list -> v_syntax -> c_unit -> (Ast_imperative.expression , _) result =
    fun ?(libs=[]) syntax c_unit ->
  parse_and_abstract_expression ~libs syntax c_unit

let compile_contract_input : ?libs: string list -> c_unit -> c_unit -> v_syntax -> (Ast_imperative.expression , _) result =
    fun ?(libs=[]) storage parameter syntax ->
  let%bind (storage,parameter) = bind_map_pair (compile_expression ~libs syntax) (storage,parameter) in
  ok @@ Ast_imperative.e_pair storage parameter

let pretty_print_cst source_filename c_unit syntax =
  pretty_print_cst syntax c_unit source_filename

let pretty_print source_filename c_unit syntax =
  pretty_print syntax c_unit source_filename
