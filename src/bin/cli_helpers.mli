open Cmdliner
open Trace

val toplevel : display_format : Main.Display.display_format -> string result -> unit Term.ret
