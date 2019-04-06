type pre_token = {
  name : string ;
  pattern : string ;
}

let make name pattern = { name ; pattern }

let keyword = fun k -> make (String.uppercase_ascii k) k
let symbol = fun sym name -> make name sym

module Print_mly = struct
  open Format

  let token = fun ppf pre_token ->
    fprintf ppf "%%token %s" pre_token.name

  let tokens = fun ppf tokens ->
    fprintf ppf "%%token EOF\n" ;
    fprintf ppf "%%token <int> INT\n" ;
    fprintf ppf "%%token <string> STRING\n" ;
    fprintf ppf "%%token <string> NAME\n" ;
    fprintf ppf "\n%a\n\n" (PP.list_sep token (PP.const "\n")) tokens ;
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
| [' ' '\t']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '"' ( [^ '"' '\\'] | ( '\\' [^ '"'] ) ) as s '"'
    { STRING s  }
|pre}
  let post =
    {post|| (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']+) as v
    { NAME v }
| eof { EOF }
| _
    { raise (Unexpected_character (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
|post}
  let tokens = fun ppf tokens ->
    fprintf ppf "%s%a\n%s" pre (PP.list_sep token (PP.const "\n")) tokens post
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
  | INT _ -> "INT n"
  | EOF -> "EOF"
|pre}

  let tokens = fun ppf tokens ->
    fprintf ppf "%s%a" pre (PP.list_sep token (PP.const "\n")) tokens
end

let tokens = [
  keyword "let" ;
  keyword "in" ;
  keyword "list" ;
  keyword "block" ;
  keyword "for" ;
  keyword "const" ;
  symbol "+" "PLUS" ;
  symbol "-" "MINUS" ;
  symbol "*" "TIMES" ;
  symbol "/" "DIV" ;
  symbol "=" "EQUAL" ;
  symbol "[" "LSQUARE" ;
  symbol "]" "RSQUARE" ;
  symbol ";" "SEMICOLON" ;
]

let () =
  let argn = Array.length Sys.argv in
  if argn = 1 then exit 1 ;
  let arg = Sys.argv.(1) in
  match arg with
  | "mll" -> (
    Format.printf "%a@.%a\n" PP.comment "Generated .mll" Print_mll.tokens tokens
  )
  | "mly" -> (
    Format.printf "%a@.%a\n" PP.comment "Generated .mly" Print_mly.tokens tokens
  )
  | "ml" -> (
    Format.printf "%a@.%a\n" PP.comment "Generated .ml" Print_ml.tokens tokens
  )
  | _ -> exit 1

