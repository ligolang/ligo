open Cmdliner
open Display

val toplevel : display_format:ex_display_format -> displayable -> ('value, Main_errors.Types.all) result -> unit Term.ret
val return_result : display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret