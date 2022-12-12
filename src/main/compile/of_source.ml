open Helpers

type file_path = string
type module_name = string
type c_unit = Buffer.t * (file_path * module_name) list

(* we should have on for filename with syntax_opt and one in case of no file *)
let extract_meta syntax : meta = { syntax }
let make_meta syntax : meta = { syntax }
let make_meta_from_syntax syntax : meta = { syntax }
let preprocess_file = Helpers.preprocess_file
let preprocess_string = Helpers.preprocess_string

let compile_string_without_preproc source : c_unit =
  let buffer = Buffer.create 0 in
  Buffer.add_string buffer source;
  buffer, []


let compile_contract_input ~raise
    : options:Compiler_options.t -> meta:meta -> string -> string -> c_unit * c_unit
  =
 fun ~options ~meta parameter storage ->
  Simple_utils.Pair.map
    ~f:(preprocess_string ~raise ~options:options.frontend ~meta)
    (parameter, storage)
