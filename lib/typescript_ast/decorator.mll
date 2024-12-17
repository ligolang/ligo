(* Lexing numbers *)

{
(* START HEADER *)

type name = string
type argument = string

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let blank   = ' ' | '\t'
let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let alphanum = letter | digit
let ident   = (small | '_'+ alphanum) (alphanum | '_')*
let string  = '"' ([^ '"' '\\' '\n']* as value) '"'

(* RULES (SCANNERS) *)

rule scan = parse
  "//" blank* { scan_decorator lexbuf }
| eof | _     { None }

and scan_decorator = parse
  '@' (ident as name) blank* { scan_argument name lexbuf }
| eof | _                    { None }

and scan_argument name = parse
 "(" blank* string blank* ")" { Some (name, Some value) }
| eof | _                     { Some (name, None) }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* END TRAILER *)
}
