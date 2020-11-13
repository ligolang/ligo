open Trace
open Helpers

type c_unit = Buffer.t

let compile ~options ~meta c_unit (source_filename:string) : (Ast_imperative.program , _) result =
  parse_and_abstract ~options ~meta c_unit source_filename

let compile_expression : options:Compiler_options.t -> meta:meta -> c_unit -> (Ast_imperative.expression , _) result =
    fun ~options ~meta c_unit ->
  parse_and_abstract_expression ~options ~meta c_unit

let compile_contract_input : options:Compiler_options.t -> meta:meta -> c_unit -> c_unit -> (Ast_imperative.expression , _) result =
    fun ~options ~meta storage parameter ->
  let%bind (storage,parameter) = bind_map_pair (compile_expression ~options ~meta) (storage,parameter) in
  ok @@ Ast_imperative.e_pair storage parameter

let pretty_print_cst ~meta source_filename c_unit =
  pretty_print_cst ~meta c_unit source_filename

let pretty_print ~meta source_filename c_unit =
  pretty_print ~meta c_unit source_filename
