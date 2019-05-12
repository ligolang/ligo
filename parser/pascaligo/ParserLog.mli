(* Printing the AST *)

val offsets : bool ref
val mode    : [`Byte | `Point] ref

val print_tokens : AST.t -> unit

val print_path : AST.path -> unit
val print_pattern : AST.pattern -> unit
val print_instruction : AST.instruction -> unit
