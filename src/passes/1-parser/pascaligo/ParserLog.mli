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

val print_tokens      : state -> AST.t -> unit
val print_path        : state -> AST.path -> unit
val print_pattern     : state -> AST.pattern -> unit
val print_instruction : state -> AST.instruction -> unit
val print_expr        : state -> AST.expr -> unit

(** {1 Printing tokens from the AST in a string} *)

val tokens_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.t -> string
val path_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.path -> string
val pattern_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.pattern -> string
val instruction_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> AST.instruction -> string

(** {1 Pretty-printing of AST nodes} *)

val pp_ast  : state -> AST.t -> unit
val pp_expr : state -> AST.expr -> unit
