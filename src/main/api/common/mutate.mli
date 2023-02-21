(* Virtual module. Will be linked in later *)
module Raw_options = Compiler_options.Raw_options

val mutate_cst
  :  Raw_options.t
  -> string
  -> Parsing.Errors.Display.ex_display_format
  -> int option
  -> bool
  -> unit
  -> (string * string, string * string) result

val mutate_ast
  :  Raw_options.t
  -> string
  -> Parsing.Errors.Display.ex_display_format
  -> int option
  -> bool
  -> unit
  -> (string * string, string * string) result
