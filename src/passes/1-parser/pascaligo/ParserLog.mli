(* Printing the AST *)

val offsets : bool ref
val mode    : [`Byte | `Point] ref

val print_tokens      : Buffer.t -> AST.t -> unit
val print_path        : Buffer.t -> AST.path -> unit
val print_pattern     : Buffer.t -> AST.pattern -> unit
val print_instruction : Buffer.t -> AST.instruction -> unit

val tokens_to_string      : AST.t -> string
val path_to_string        : AST.path -> string
val pattern_to_string     : AST.pattern -> string
val instruction_to_string : AST.instruction -> string

(* Pretty-printing of the AST *)

val pp_ast : Buffer.t -> AST.t -> unit
