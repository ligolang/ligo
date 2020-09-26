open Cmdliner
open Display


val toplevel : input:input_type -> ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, Main_errors.Types.all) result -> unit Term.ret
val return_result : input:input_type -> ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret