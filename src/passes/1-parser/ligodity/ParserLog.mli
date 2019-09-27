(* Printing the AST *)

val offsets : bool ref
val mode    : [`Byte | `Point] ref

(* Printing the tokens reconstructed from the AST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). The
   optional parameter [undo] is bound to [true] if the caller wants
   the AST to be unparsed before printing (those nodes that have been
   normalised with function [norm_let] and [norm_fun]). *)

val print_tokens : AST.t -> unit

(* val print_path : AST.path -> unit *)
val print_pattern : AST.pattern -> unit
val print_expr : AST.expr -> unit
(* val print_instruction : AST.instruction -> unit *)

(* val print_projection : projection -> unit
val print_pattern : pattern -> unit
val print_expr : expr -> unit *)
