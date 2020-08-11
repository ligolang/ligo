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

  "And"      -> "and"
| "Begin"    -> "begin"
| "BigMap"   -> "big_map"
| "Block"    -> "block"
| "Case"     -> "case"
| "Const"    -> "const"
| "Contains" -> "contains"
| "Else"     -> "else"
| "End"      -> "end"
| "False"    -> "False"
| "For"      -> "for"
| "Function" -> "function"
| "From"     -> "from"
| "If"       -> "if"
| "In"       -> "in"
| "Is"       -> "is"
| "List"     -> "list"
| "Map"      -> "map"
| "Mod"      -> "mod"
| "Nil"      -> "nil"
| "Not"      -> "not"
| "Of"       -> "of"
| "Or"       -> "or"
| "Patch"    -> "patch"
| "Record"   -> "record"
| "Remove"   -> "remove"
| "Set"      -> "set"
| "Skip"     -> "skip"
| "Then"     -> "then"
| "To"       -> "to"
| "True"     -> "True"
| "Type"     -> "type"
| "Unit"     -> "Unit"
| "Var"      -> "var"
| "While"    -> "while"
| "With"     -> "with"

  (* Data constructors *)

| "C_None"   -> "None"
| "C_Some"   -> "Some"

  (* Symbols *)

| "SEMI"     -> ";"
| "COMMA"    -> ","
| "LPAR"     -> "("
| "RPAR"     -> ")"
| "LBRACE"   -> "{"
| "RBRACE"   -> "}"
| "LBRACKET" -> "["
| "RBRACKET" -> "]"
| "CONS"     -> "#"
| "VBAR"     -> "|"
| "ARROW"    -> "->"
| "ASS"      -> ":="
| "EQ"       -> "="
| "COLON"    -> ":"
| "LT"       -> "<"
| "LE"       -> "<="
| "GT"       -> ">"
| "GE"       -> ">="
| "NE"       -> "=/="
| "PLUS"     -> "+"
| "MINUS"    -> "  -"
| "SLASH"    -> "/"
| "TIMES"    -> "*"
| "DOT"      -> "."
| "WILD"     -> "_"
| "CAT"      -> "^"

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
