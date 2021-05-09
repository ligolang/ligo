open Trace
open Helpers

type file_path = string
type module_name = string

type c_unit = Buffer.t * (file_path * module_name) list

(* we should have on for filename with syntax_opt and one in case of no file *)
let extract_meta syntax file_name =
  let* syntax   = syntax_to_variant (Syntax_name syntax) (Some file_name) in
  ok @@ {syntax}

let make_meta syntax file_name_opt =
  let* syntax   = syntax_to_variant (Syntax_name syntax) file_name_opt in
  ok @@ {syntax}

let make_meta_from_syntax syntax =
  ok @@ {syntax}

let compile ~options ~meta (source_filename:string) : (c_unit , _) result =
  preprocess_file ~options ~meta source_filename

let compile_string ~options ~meta source : (c_unit , _) result =
  preprocess_string ~options ~meta source

let compile_string_without_preproc source : (c_unit , _) result =
  let buffer = Buffer.create 0 in
  Buffer.add_string buffer source;
  ok @@ (buffer, [])

let compile_contract_input : options:Compiler_options.t -> meta:meta -> string -> string -> (c_unit * c_unit , _) result =
    fun ~options ~meta parameter storage ->
  bind_map_pair (compile_string ~options ~meta) (parameter,storage)
