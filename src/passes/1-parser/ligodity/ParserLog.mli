(* Printing the AST *)

val offsets : bool ref
val mode    : [`Byte | `Point] ref

(* Printing the tokens reconstructed from the AST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). The
   optional parameter [undo] is bound to [true] if the caller wants
   the AST to be unparsed before printing (those nodes that have been
   normalised with function [norm_let] and [norm_fun]). *)

val print_tokens  : Buffer.t -> AST.t -> unit
val print_pattern : Buffer.t -> AST.pattern -> unit
val print_expr    : Buffer.t -> AST.expr -> unit

val tokens_to_string  : AST.t -> string
val pattern_to_string : AST.pattern -> string
val expr_to_string    : AST.expr -> string
