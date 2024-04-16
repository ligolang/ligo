(* Status after parsing the CLI *)

(* Note that [`Help] and [`CLI] take an argument of type [Buffer.t]
   instead of [string]. This because further passes (lexing is the
   next) will add their commands. *)

type t = [
  `Done
| `Version      of string
| `Help         of Buffer.t
| `CLI          of Buffer.t
| `SyntaxError  of string
| `FileNotFound of string
| `WrongFileExt of string
]

type status = t

val status : t
