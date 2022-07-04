module Trace = Simple_utils.Trace

val of_string_opt :
  raise : (Main_errors.all,Main_warnings.all) Trace.raise ->
  ?dialect : Syntax_types.s_dialect ->
  Syntax_types.s_syntax ->
  string option ->
  Syntax_types.t

val to_string : Syntax_types.t -> string
