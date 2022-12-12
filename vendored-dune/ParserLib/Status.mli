(* Status after parsing the CLI *)

type t = [
  LexerLib.Status.t             (* Lexer CLI options *)
| `DependsOn of string * string (* The first option depends on the second *)
]

type status = t

val status : t
