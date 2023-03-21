open Simple_utils
module LMap : Map.S with type key = Location.t

type t = Types.type_case LMap.t

val resolve
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> stdlib_decls:Ast_typed.program
  -> Ast_core.program
  -> t
