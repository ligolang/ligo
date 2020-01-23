(** Converting the textual representation of tokens produced by Menhir
    into concrete syntax *)

(* See [ParToken.mly] *)

let gen_sym prefix =
  let count = ref 0 in
  fun () -> incr count;
         prefix ^ string_of_int !count

let id_sym   = gen_sym "id"
and ctor_sym = gen_sym "C"

let concrete = function
  (* Keywords *)

| "Else"   -> "else"
| "False"  -> "false"
| "If"     -> "if"
| "Let"    -> "let"
| "Switch" -> "switch"
| "Mod"    -> "mod"
| "Or"     -> "or"
| "True"   -> "true"
| "Type"   -> "type"

  (* Data constructors *)

| "C_None"   -> "None"
| "C_Some"   -> "Some"

  (* Symbols *)

| "MINUS"    -> "-"
| "PLUS"     -> "+"
| "SLASH"    -> "/"
| "TIMES"    -> "*"

| "LPAR"     -> "("
| "RPAR"     -> ")"
| "LBRACKET" -> "["
| "RBRACKET" -> "]"
| "LBRACE"   -> "{"
| "RBRACE"   -> "}"

| "CAT"      -> "++"
| "DOT"      -> "."
| "ELLIPSIS" -> "..."

| "COMMA"    -> ","
| "SEMI"     -> ";"
| "COLON"    -> ":"
| "VBAR"     -> "|"

| "WILD"     -> "_"

| "EQ"       -> "="
| "EQEQ"     -> "=="
| "NE"       -> "!="
| "LT"       -> "<"
| "GT"       -> ">"
| "LE"       -> "<="
| "GE"       -> ">="
| "ARROW"    -> "=>"

| "NOT"      -> "!"
| "BOOL_OR"  -> "||"
| "BOOL_AND" -> "&&"

  (* Literals *)

| "String"   -> "\"a string\""
| "Bytes"    -> "0xAA"
| "Int"      -> "1"
| "Nat"      -> "1n"
| "Mutez"    -> "1mutez"
| "Ident"    -> id_sym ()
| "Constr"   -> ctor_sym ()

  (* Virtual tokens *)

| "EOF"      -> ""

  (* For completeness of open sum types *)

| _          -> "<Unknown>"

(* Unlexing a sentence *)

let unlex (sentence: string) : Buffer.t =
  let tokens  = Str.split (Str.regexp " ") sentence in
  let lexemes = List.map concrete tokens in
  let buffer  = Buffer.create 31 in
  let rec trans = function
      [] -> ()
  |  [s] -> Buffer.add_string buffer s
  | s::l -> Buffer.add_string buffer (s ^ " "); trans l
  in trans lexemes; buffer

(* Reading one line from the standard input channel and unlex it. *)

let out = unlex (input_line stdin) |> Buffer.contents
let ()  = Printf.printf "%s\n" out
