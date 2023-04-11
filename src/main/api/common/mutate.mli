(* Virtual module. Will be linked in later *)
open Compiler_options
open Simple_utils.Trace

val mutate_cst
  :  Raw_options.t
  -> string
  -> int option
  -> Buffer.t Simple_utils.Display.format
     * (raise:(Main_errors.all, Main_warnings.all) raise
        -> Buffer.t * Analytics.analytics_inputs)
