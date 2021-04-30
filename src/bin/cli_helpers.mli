open Cmdliner
open Display

val toplevel : ?werror:bool -> ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, Main_errors.all) Trace.result -> unit Term.ret
val return_result : ?werror:bool -> ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.all) Trace.result -> unit Term.ret
