open Cmdliner
open Display

val toplevel : ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, Build.Errors.build_error) Trace.result -> unit Term.ret
val return_result : ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Build.Errors.build_error) Trace.result -> unit Term.ret
