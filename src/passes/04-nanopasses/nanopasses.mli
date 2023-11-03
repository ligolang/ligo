module Errors = Passes.Errors
module Selector = Passes.Pass_type.Selector

(* [execute_nanopasses] only execute the pass list until it reaches the [stop_before]
   pass (corresponds to the module name of that pass)
   [disable_initial_check] disable the pass responsible to prevent initial nodes
   to be present in the initial value
*)
val execute_nanopasses
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> sort:'a Selector.t
  -> ?stop_before:string
  -> ?disable_initial_check:bool
  -> 'a
  -> 'a

val compile_program
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> ?stop_before:string
  -> Ast_unified.program
  -> Ast_core.program

(* disable_initial_check is used in test, where we emit final node right away *)
val compile_expression
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> ?disable_initial_check:bool
  -> Ast_unified.expr
  -> Ast_core.expression

val decompile_pattern
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> syntax:Syntax_types.t
  -> Ast_core.type_expression option Ast_core.Pattern.t
  -> Ast_unified.pattern

val decompile_ty_expr
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> syntax:Syntax_types.t
  -> Ast_core.type_expression
  -> Ast_unified.ty_expr

val decompile_program
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> syntax:Syntax_types.t
  -> Ast_core.program
  -> Ast_unified.program

val decompile_expression
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> syntax:Syntax_types.t
  -> Ast_core.expression
  -> Ast_unified.expr

val decompile_sig_expr
  :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
  -> syntax:Syntax_types.t
  -> Ast_core.signature_expr
  -> Ast_unified.sig_expr
