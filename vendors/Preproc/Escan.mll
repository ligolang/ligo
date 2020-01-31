{
(* Auxiliary scanner for boolean expressions of the C# preprocessor *)

(* Concrete syntax of tokens. See module [Eparser]. *)

let string_of_token =
  let open Eparser
in function True -> "true"
      |    False -> "false"
      | Ident id -> id
      |       OR -> "||"
      |      AND -> "&&"
      |       EQ -> "=="
      |      NEQ -> "!="
      |      NOT -> "!"
      |     LPAR -> "("
      |     RPAR -> ")"
      |      EOL -> "EOL"

}

(* Regular expressions for literals *)

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Unicode escape sequences *)

let digit = ['0'-'9']
let hexdigit = digit | ['A'-'F' 'a'-'f']
let four_hex = hexdigit hexdigit hexdigit hexdigit
let uni_esc = "\\u" four_hex | "\\U"  four_hex four_hex

(* Identifiers *)

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase | uni_esc
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* Rules *)

rule token = parse
  blank+      { token lexbuf      }
| newline     { Lexing.new_line lexbuf; Eparser.EOL }
| eof         { Eparser.EOL       }
| "true"      { Eparser.True      }
| "false"     { Eparser.False     }
| ident as id { Eparser.Ident id  }
| '('         { Eparser.LPAR      }
| ')'         { Eparser.RPAR      }
| "||"        { Eparser.OR        }
| "&&"        { Eparser.AND       }
| "=="        { Eparser.EQ        }
| "!="        { Eparser.NEQ       }
| "!"         { Eparser.NOT       }
| "//"        { inline_com lexbuf }
| _ as c      { let code = Char.code c in
                let msg = "Invalid character " ^ String.make 1 c
                          ^ " (" ^ string_of_int code ^ ")."
                in raise Error.(Lexer (msg, mk_seg lexbuf, 1))
              }

and inline_com = parse
  newline { Lexing.new_line lexbuf; Eparser.EOL }
| eof     { Eparser.EOL }
| _       { inline_com lexbuf }

{
(* Standalone lexer for debugging purposes. See module [Topexp]. *)

type filename = string

let trace (name: filename) =
  match open_in name with
    cin ->
      let buffer = Lexing.from_channel cin
      and cout = stdout in
      let rec iter () =
        match token buffer with
          Eparser.EOL -> close_in cin; close_out cout
        |           t -> begin
                          output_string cout (string_of_token t);
                          output_string cout "\n";
                          flush cout;
                          iter ()
                        end
        | exception Error.Lexer diag -> Error.print "Lexical" diag
      in iter ()
  | exception Sys_error msg -> prerr_endline msg
}
