(** Printing the AST *)

(** The type [state] captures the state that is threaded in the
    printing iterators in this module.
*)
type state

val mk_state :
  offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

(** {1 Printing tokens from the AST in a buffer}

   Printing the tokens reconstructed from the AST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). *)

val print_tokens  : state -> AST.t -> unit
val print_pattern : state -> AST.pattern -> unit
val print_expr    : state -> AST.expr -> unit

val tokens_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.t -> string
val pattern_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.pattern -> string
val expr_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.expr -> string

(** {1 Pretty-printing of the AST} *)

val pp_ast : state -> AST.t -> unit
