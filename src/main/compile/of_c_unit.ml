open Trace
open Helpers

type c_unit = Buffer.t

let compile ~meta c_unit (source_filename:string) : (Ast_imperative.module_ , _) result =
  parse_and_abstract ~meta c_unit source_filename

let compile_expression = parse_and_abstract_expression

let compile_string : meta:meta -> c_unit -> (Ast_imperative.module_ , _) result =
    fun ~meta c_unit ->
  parse_and_abstract_string meta.syntax c_unit

let compile_contract_input : meta:meta -> c_unit -> c_unit -> (Ast_imperative.expression , _) result =
    fun ~meta storage parameter ->
  let* (storage,parameter) = bind_map_pair (compile_expression ~meta) (storage,parameter) in
  ok @@ Ast_imperative.e_pair storage parameter

let pretty_print_cst = pretty_print_cst

let pretty_print = pretty_print
