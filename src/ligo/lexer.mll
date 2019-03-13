{
  open Parser

  exception Error of string
  exception Unexpected_character of string
}

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

rule token = parse
(*
  | "//" ([^ '\n']* ) (['\n' '\r']+)
    { Lexing.new_line lexbuf ; token lexbuf }
*)
| ('\r'? '\n' '\r'?)
  { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t']
    { token lexbuf }
| '"' ( [^ '"' '\\'] | ( '\\' [^ '"'] ) ) as s '"'
    { STRING s  }
| "let" { LET }
| "if" { IF }
(* | "then" { THEN } *)
| "elseif" { ELSEIF }
| "else" { ELSE }
(* | "in" { IN } *)
| "type" { TYPE }
| "function" { FUNCTION }
| "while"
    { WHILE }
| "foreach"
    { FOREACH }
| "of"
    { OF }
| (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']+) as v
    { VAR_NAME v }
| (['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']+) as t
    { TYPE_NAME t }
(* | ['0'-'9']+'.'['0'-'9']* as i { FLOAT (float_of_string i) } *)
| ['0'-'9']+ as i
    { INT (int_of_string i) }
(*
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | ";;" { DOUBLE_SEMICOLON }
*)
| '=' { EQUAL }
| ',' { COMMA }
| ';' { SEMICOLON }
| ':' { COLON }
| '&'
    { AND }
| '|'
    { AND }
| '.'
    { DOT }
| '@'
    { AT }
| '('
    { LPAREN }
| ')'
    { RPAREN }
(*
  | '[' { LSQUARE }
  | ']' { RSQUARE }
*)
| '{'
    { LBRACKET }
| '}'
    { RBRACKET }
| eof { EOF }
| _
    { raise (Unexpected_character (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
