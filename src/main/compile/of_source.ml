open Trace
open Helpers

type file_path = string
type module_name = string

type c_unit = Buffer.t * (file_path * module_name) list

let compile ?(libs=[]) (source_filename:string) syntax : (c_unit , _) result =
  let%bind syntax   = syntax_to_variant syntax (Some source_filename) in
  preprocess_file ~libs syntax source_filename


let compile_string ?(libs=[]) syntax source : (c_unit , _) result =
  preprocess_string ~libs syntax source

let compile_contract_input : ?libs: string list -> string -> string -> v_syntax -> (c_unit * c_unit , _) result =
    fun ?(libs=[]) storage parameter syntax ->
  bind_map_pair (compile_string ~libs syntax) (storage,parameter)
