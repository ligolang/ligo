
(* The type of tokens. *)

type token = 
  | TIMES
  | STRING of (string)
  | SEMICOLON
  | RSQUARE
  | PLUS
  | NAME of (string)
  | MINUS
  | LSQUARE
  | LIST
  | LET
  | INT of (int)
  | IN
  | EQUAL
  | EOF
  | DIV
