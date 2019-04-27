type pre_token = {
  name : string ;
  pattern : string ;
}

let make name pattern = { name ; pattern }

let keyword = fun k ->
  let regexp = Str.regexp "[^0-9a-zA-Z]" in
  let constructor_name =
    Str.global_replace regexp "_"
    @@ String.uppercase_ascii k
  in
  make constructor_name k
let symbol = fun sym name -> make name sym

module Print_mly = struct
  open Format

  let token = fun ppf pre_token ->
    fprintf ppf "%%token %s" pre_token.name

  let tokens = fun ppf tokens ->
    fprintf ppf "%%token EOF\n" ;
    fprintf ppf "%%token <int> INT\n" ;
    fprintf ppf "%%token <int> TZ\n" ;
    fprintf ppf "%%token <string> STRING\n" ;
    fprintf ppf "%%token <string> NAME\n" ;
    fprintf ppf "%%token <string> CONSTRUCTOR_NAME\n" ;
    fprintf ppf "\n%a\n\n" (PP_helpers.list_sep token (PP_helpers.const "\n")) tokens ;
    fprintf ppf "%%%%\n"
end

module Print_mll = struct
  open Format

  let token = fun ppf {name;pattern} ->
    fprintf ppf "| \"%s\" { %s }" pattern name

  let pre =
    {pre|{
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
| '"' { string "" lexbuf }
| [' ' '\t']
    { token lexbuf }
| (['0'-'9']+ as n) '.' (['0'-'9']['0'-'9'] as d) "tz" { TZ ((int_of_string n) * 100 + (int_of_string d)) }
| (['0'-'9']+ as i) 'p'?
    { INT (int_of_string i) }
|pre}
  let post =
    {post|
| (['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*) as v
    { NAME v }
| (['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*) as v
    { CONSTRUCTOR_NAME v }
| eof { EOF }
| "(*" { comment 1 lexbuf }
| _
    { raise (Unexpected_character (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and string s = parse
  | "\\\"" { string (s ^ "\"") lexbuf }
  | "\\\\" { string (s ^ "\\") lexbuf }
  | '"' { STRING s }
  | eof                         { raise (Unexpected_character "missing string terminator") }
  | _ as c                      { string (s ^ (String.make 1 c)) lexbuf }


and comment n = parse
  | "*)"                        { if n = 1 then token lexbuf else comment (n - 1) lexbuf }
  | "(*"                        { comment (n + 1) lexbuf }
  | '"' ( [^ '"' '\\'] | ( '\\' [^ '"'] ) ) '"' { comment n lexbuf }
  | eof                         { raise (Unexpected_character "missing comment terminator") }
  | ('\r'? '\n' '\r'?)          { Lexing.new_line lexbuf; comment n lexbuf }
  | _                           { comment n lexbuf }

|post}
  let tokens = fun ppf tokens ->
    fprintf ppf "%s%a\n%s" pre (PP_helpers.list_sep token (PP_helpers.const "\n")) tokens post
end

module Print_ml = struct
  open Format

  let token = fun ppf {name} ->
    fprintf ppf "  | %s -> \"%s\"" name name

  let pre =
    {pre|include Token_type

let to_string : token -> string = function
  | STRING _ -> "STRING"
  | NAME _ -> "NAME s"
  | CONSTRUCTOR_NAME _ -> "CONSTRUCTOR_NAME s"
  | INT _ -> "INT n"
  | TZ _ -> "TZ n"
  | EOF -> "EOF"
|pre}

  let tokens = fun ppf tokens ->
    fprintf ppf "%s%a" pre (PP_helpers.list_sep token (PP_helpers.const "\n")) tokens
end

let tokens = [
  keyword "let%init" ;
  keyword "let%entry" ;
  keyword "let" ;
  keyword "type" ;
  keyword "in" ;
  keyword "if" ;
  keyword "then" ;
  keyword "else" ;
  (* keyword "block" ;
   * keyword "for" ;
   * keyword "const" ; *)
  keyword "fun" ;
  keyword "match" ;
  keyword "with" ;
  symbol "()" "UNIT" ;
  symbol "+" "PLUS" ;
  symbol "~" "TILDE" ;
  symbol "->" "ARROW" ;
  symbol "<-" "LEFT_ARROW" ;
  symbol "<=" "LE" ;
  symbol "<>" "UNEQUAL" ;
  symbol "<" "LT" ;
  symbol ">" "GT" ;
  symbol "-" "MINUS" ;
  symbol "*" "TIMES" ;
  symbol "/" "DIV" ;
  symbol "=" "EQUAL" ;
  symbol "|" "VBAR" ;
  symbol "[" "LSQUARE" ;
  symbol "]" "RSQUARE" ;
  symbol "(" "LPAREN" ;
  symbol ")" "RPAREN" ;
  symbol "{" "LBRACKET" ;
  symbol "}" "RBRACKET" ;
  symbol ";;" "DOUBLE_SEMICOLON" ;
  symbol ";" "SEMICOLON" ;
  symbol "::" "DOUBLE_COLON" ;
  symbol ":" "COLON" ;
  symbol "," "COMMA" ;
  symbol "." "DOT" ;
]

let () =
  let argn = Array.length Sys.argv in
  if argn = 1 then exit 1 ;
  let arg = Sys.argv.(1) in
  match arg with
  | "mll" -> (
    Format.printf "%a@.%a\n" PP_helpers.comment "Generated .mll" Print_mll.tokens tokens
  )
  | "mly" -> (
    Format.printf "%a@.%a\n" PP_helpers.comment "Generated .mly" Print_mly.tokens tokens
  )
  | "ml" -> (
    Format.printf "%a@.%a\n" PP_helpers.comment "Generated .ml" Print_ml.tokens tokens
  )
  | _ -> exit 1

