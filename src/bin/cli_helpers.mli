open Cmdliner
open Display

val toplevel : ?output_file:string option -> brief:bool -> display_format:ex_display_format -> displayable -> ('value, Main_errors.Types.all) result -> unit Term.ret
val return_result : ?output_file:string option -> brief:bool -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret