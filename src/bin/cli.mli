(*
open Cmdliner

val main : unit Term.t * Term.info

val source : int -> string Term.t

val entry_point : int -> string Term.t

val expression : string  -> int -> string Term.t

val syntax : string Term.t

val amount : string Term.t

val compile_file : unit Term.t * Term.info

val compile_parameter : unit Term.t * Term.info

val compile_storage : unit Term.t * Term.info

val dry_run : unit Term.t * Term.info

val run_function : unit Term.t * Term.info

val evaluate_value : unit Term.t * Term.info
*)
