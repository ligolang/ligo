module Location = Simple_utils.Location
open Helpers

type c_unit = Buffer.t

let compile ~raise ~meta ~preprocess_define c_unit (source_filename : string)
    : Ast_unified.program
  =
  parse_and_abstract ~raise ~meta ~preprocess_define c_unit source_filename


let compile_expression ~raise ~preprocess_define =
  parse_and_abstract_expression ~raise ~preprocess_define


let compile_type_expression ~raise = parse_and_abstract_type_expression ~raise

let compile_string ~raise
    : meta:meta -> preprocess_define:string list -> c_unit -> Ast_unified.program
  =
 fun ~meta ~preprocess_define c_unit ->
  parse_and_abstract_string ~raise ~preprocess_define meta.syntax c_unit


let compile_contract_input ~raise
    : meta:meta -> preprocess_define:string list -> c_unit -> c_unit -> Ast_unified.expr
  =
 fun ~meta ~preprocess_define storage parameter ->
  let storage, parameter =
    Simple_utils.Pair.map
      ~f:(compile_expression ~raise ~meta ~preprocess_define)
      (storage, parameter)
  in
  Ast_unified.e_tuple
    ~loc:Location.dummy
    (Simple_utils.List.Ne.of_list [ storage; parameter ])


let pretty_print_cst = pretty_print_cst
let pretty_print = pretty_print
