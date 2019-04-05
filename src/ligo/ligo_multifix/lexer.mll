{
  open Token

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
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '"' ( [^ '"' '\\'] | ( '\\' [^ '"'] ) ) as s '"'
    { STRING s  }
| (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']+) as v
    { NAME v }
| "let" { LET }
| "in" { IN }
| "list" { LIST }
| "[" { LSQUARE }
| "]" { RSQUARE }
| ";" { SEMICOLON }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIV }
| "=" { EQUAL }
| eof { EOF }
| _
    { raise (Unexpected_character (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
