(* Lexer specification for Mini-ML, to be processed by [ocamllex]. *)

{
(* START HEADER *)

(* UTILITIES *)

let sprintf = Printf.sprintf

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module SMap   = Utils.String.Map

(* Making a natural from its decimal notation (for Tez) *)

let format_tz s =
  match String.index s '.' with
    index ->
      let len         = String.length s in
      let integral    = Str.first_chars s index
      and fractional  = Str.last_chars s (len-index-1) in
      let num         = Z.of_string (integral ^ fractional)
      and den         = Z.of_string ("1" ^ String.make (len-index-1) '0')
      and million     = Q.of_string "1000000" in
      let mtz         = Q.make num den |> Q.mul million in
      let should_be_1 = Q.den mtz in
      if Z.equal Z.one should_be_1 then Some (Q.num mtz) else None
  | exception Not_found -> assert false

(* STRING PROCESSING *)

(* The value of [mk_str len p] ("make string") is a string of length
   [len] containing the [len] characters in the list [p], in reverse
   order. For instance, [mk_str 3 ['c';'b';'a'] = "abc"]. *)

let mk_str (len: int) (p: char list) : string =
  let bytes = Bytes.make len ' ' in
  let rec fill i = function
         [] -> bytes
  | char::l -> Bytes.set bytes i char; fill (i-1) l
  in fill (len-1) p |> Bytes.to_string

(* The call [explode s a] is the list made by pushing the characters
   in the string [s] on top of [a], in reverse order. For example,
   [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

let explode s acc =
  let rec push = function
    0 -> acc
  | i -> s.[i-1] :: push (i-1)
in push (String.length s)

type thread = {
  opening : Region.t;
  len     : int;
  acc     : char list
}

let push_char char {opening; len; acc} =
  {opening; len=len+1; acc=char::acc}

let push_string str {opening; len; acc} =
  {opening;
   len = len + String.length str;
   acc = explode str acc}

(* LEXICAL ERRORS *)

type message = string

exception Error of message Region.reg

let error lexbuf msg =
  let start  = Pos.from_byte (Lexing.lexeme_start_p lexbuf)
  and stop   = Pos.from_byte (Lexing.lexeme_end_p   lexbuf) in
  let region = Region.make ~start ~stop
  in raise (Error Region.{region; value=msg})

let fail region value = raise (Error Region.{region; value})

(* KEYWORDS *)

let keywords = Token.[
  "begin",  Some Begin;
  "else",   Some Else;
  "false",  Some False;
  "fun",    Some Fun;
  "if",     Some If;
  "in",     Some In;
  "end",    Some End;
  "let",    Some Let;
  "match",  Some Match;
  "mod",    Some Mod;
  "not",    Some Not;
  "of",     Some Of;
  "or",     Some Or;
  "then",   Some Then;
  "true",   Some True;
  "type",   Some Type;
  "with",   Some With;

  (* Reserved *)

  "and",         None;
  "as",          None;
  "asr",         None;
  "assert",      None;
  "class",       None;
  "constraint",  None;
  "do",          None;
  "done",        None;
  "downto",      None;
  "exception",   None;
  "external",    None;
  "for",         None;
  "function",    None;
  "functor",     None;
  "include",     None;
  "inherit",     None;
  "initializer", None;
  "land",        None;
  "lazy",        None;
  "lor",         None;
  "lsl",         None;
  "lsr",         None;
  "lxor",        None;
  "method",      None;
  "module",      None;
  "mutable",     None;
  "new",         None;
  "nonrec",      None;
  "object",      None;
  "open",        None;
  "private",     None;
  "rec",         None;
  "sig",         None;
  "struct",      None;
  "to",          None;
  "try",         None;
  "val",         None;
  "virtual",     None;
  "when",        None;
  "while",       None
]

let add map (key,value) = SMap.add key value map

let kwd_map = List.fold_left add SMap.empty keywords

(* LEXER ENGINE *)

(* Resetting file name and line number (according to #line directives) *)

let reset_file ~file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line ~line buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_lnum = line}

let reset_offset ~offset buffer =
  assert (offset >= 0);
  let open Lexing in
  let bol = buffer.lex_curr_p.pos_bol in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?line ?offset buffer =
  let () =
    match file with
      Some file -> reset_file ~file buffer
    |      None -> () in
  let () =
    match line with
      Some line -> reset_line ~line buffer
    |      None -> () in
  match offset with
    Some offset -> reset_offset ~offset buffer
  |        None -> ()

(* Hack to roll back one lexeme in the current semantic action *)
(*
let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <-
    {lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - len}
*)

(* REGIONS *)

let mk_region start stop =
  let start = Pos.from_byte start
  and stop  = Pos.from_byte stop
  in Region.make ~start ~stop

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Auxiliary regular expressions *)

let nl       = ['\n' '\r']
let blank    = [' ' '\t']

let digit    = ['0'-'9']
let natural  = digit | digit (digit | '_')* digit
let integer  = '-'? natural
let decimal  = digit+ '.' digit+

let small    = ['a'-'z']
let capital  = ['A'-'Z']
let letter   = small | capital

let ichar    = letter | digit | ['_' '\'']
let ident    = small ichar* | '_' ichar+
let uident   = capital ichar*
let tparam   = "'" ident (* Type parameters. Unused yet *)

let hexa     = digit | ['A'-'F']
let byte     = hexa hexa
let byte_seq = byte | byte (byte | '_')* byte
let bytes    = "0x" (byte_seq? as seq)

let esc      = "\\n" | "\\\\" | "\\b" | "\\r" | "\\t"
let schar    = [^'"' '\\'] # nl (* TODO: Test *)
               | "\\\"" | esc | "\\x" byte | "\\0" digit digit
let string   = '"' schar* '"'
let char_set = [^'\'' '\\'] # nl (* TODO: Test *)
               | "\\'" | esc | "\\x" byte | "\\0" digit digit
let char     = "'" char_set "'"

(* Rules *)

rule scan = parse
  nl     { Lexing.new_line lexbuf; scan lexbuf }
| blank+ { scan lexbuf    }

| "->"   { Token.ARROW    }
| "::"   { Token.CONS     }
| "^"    { Token.CAT      }
  (*| "@"    { Token.APPEND   }*)

| "="    { Token.EQ       }
| "<>"   { Token.NE       }
| "<"    { Token.LT       }
| ">"    { Token.GT       }
| "<="   { Token.LE       }
| ">="   { Token.GE       }

| "&&"   { Token.BOOL_AND }
| "||"   { Token.BOOL_OR  }

| "-"    { Token.MINUS    }
| "+"    { Token.PLUS     }
| "/"    { Token.SLASH    }
| "*"    { Token.TIMES    }

| "("    { Token.LPAR     }
| ")"    { Token.RPAR     }
| "["    { Token.LBRACKET }
| "]"    { Token.RBRACKET }
| "{"    { Token.LBRACE   }
| "}"    { Token.RBRACE   }

| ","    { Token.COMMA    }
| ";"    { Token.SEMI     }
| ":"    { Token.COLON    }
| "|"    { Token.VBAR     }
| "."    { Token.DOT      }

| "_"    { Token.WILD     }
| eof    { Token.EOF      }

| integer as n       { Token.Int (n, Z.of_string n)          }
| integer as n "p"   { Token.Nat (n ^ "p", Z.of_string n)    }
| integer as tz "tz" { Token.Mtz (tz ^ "tz", Z.of_string tz) }
| decimal as tz "tz" {
    match format_tz tz with
      Some z -> Token.Mtz (tz ^ "tz", z)
    | None   -> sprintf "Invalid tez amount." |> error lexbuf
  }
| uident as id { Token.Constr id }
| bytes {
    let norm = Str.(global_replace (regexp "_") "" seq)
    in Token.Bytes (seq, Hex.of_string norm)
  }
| "let%init"    { Token.Let                    }
| "let%entry"   { Token.LetEntry               }
| "match%nat"   { Token.MatchNat               }
| ident   as id {
    match SMap.find id kwd_map with
      None -> sprintf "Reserved name \"%s\"." id |> error lexbuf
    | Some kwd -> kwd
    | exception Not_found -> Token.Ident id }

| '"'  { let start   = Lexing.lexeme_start_p lexbuf
         and stop    = Lexing.lexeme_end_p lexbuf in
         let opening = mk_region start stop in
         let thread  = {opening; len=1; acc=['"']} in
         let thread  = scan_string thread lexbuf in
         let lexeme  = mk_str thread.len thread.acc in
         let      () = lexbuf.Lexing.lex_start_p <- start
         in Token.Str lexeme }

| "(*" { let start   = Lexing.lexeme_start_p lexbuf
         and stop    = Lexing.lexeme_end_p lexbuf in
         let opening = mk_region start stop in
         let thread  = {opening; len=2; acc=['*';'(']} in
         let thread  = scan_block thread lexbuf in
         let ()      = ignore thread
         in scan lexbuf }

  (* Management of #include CPP directives

    An input LIGO program may contain GNU CPP (C preprocessor)
    directives, and the entry modules (named *Main.ml) run CPP on them
    in traditional mode:

    https://gcc.gnu.org/onlinedocs/cpp/Traditional-Mode.html

      The main interest in using CPP is that it can stand for a poor
    man's (flat) module system for LIGO thanks to #include
    directives, and the traditional mode leaves the markup mostly
    undisturbed.

      Some of the #line resulting from processing #include directives
    deal with system file headers and thus have to be ignored for our
    purpose. Moreover, these #line directives may also carry some
    additional flags:

    https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

    of which 1 and 2 indicate, respectively, the start of a new file
    and the return from a file (after its inclusion has been
    processed).
  *)

| '#' blank* ("line" blank+)? (integer as line) blank+
    '"' (string as file) '"' {
    let flags = scan_flags [] lexbuf in
    let    () = ignore flags in
    let line  = int_of_string line
    and file  = Filename.basename file in
    let    () = reset ~file ~line ~offset:0 lexbuf
    in scan lexbuf
  }

| _ as c { let msg = sprintf "Invalid character '%s'."
                       (Char.escaped c)
           in error lexbuf msg }

(* Scanning CPP #include flags *)

and scan_flags acc = parse
  blank+          { scan_flags acc lexbuf                  }
| integer as code { let acc = int_of_string code :: acc
                    in scan_flags acc lexbuf               }
| nl              { Lexing.new_line lexbuf; List.rev acc   }
| eof             { List.rev acc                           }

(* Finishing a string *)

and scan_string thread = parse
  nl       { fail thread.opening "Broken string."          }
| eof      { fail thread.opening "Unterminated string."    }
| '"'      { push_char '"' thread }
| esc as s { scan_string (push_string s thread) lexbuf     }
| '\\' _   { let start  = Lexing.lexeme_start_p lexbuf
             and stop   = Lexing.lexeme_end_p lexbuf in
             let region = mk_region start stop
             in fail region "Undefined escape sequence."   }
| _ as c   { scan_string (push_char c thread) lexbuf       }

(* Comments *)

and scan_block thread = parse
  '"' | "(*" {
           let opening  = thread.opening in
           let start    = Lexing.lexeme_start_p lexbuf
           and stop     = Lexing.lexeme_end_p lexbuf in
           let opening' = mk_region start stop in
           let lexeme   = Lexing.lexeme lexbuf in
           let thread   = push_string lexeme thread in
           let thread   = {thread with opening=opening'} in
           let next     = if lexeme = "\"" then scan_string
                          else scan_block in
           let thread   = next thread lexbuf in
           let thread   = {thread with opening}
           in scan_block thread lexbuf                      }
| "*)"   { push_string (Lexing.lexeme lexbuf) thread        }
| nl     { Lexing.new_line lexbuf; scan_block thread lexbuf }
| eof    { fail thread.opening "Open comment."              }
| _ as c { scan_block (push_char c thread) lexbuf           }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

type logger = out_channel * (out_channel -> Token.t -> unit)

let get_token ?log =
  match log with
    None -> scan
  | Some (out_chan, print) ->
      let print = print out_chan in
      fun buffer -> let t = scan buffer in print t; flush out_chan; t

(* Standalone lexer for debugging purposes *)

(* TODO: Move out (functor). See LIGO. *)

let format_error ~(kind: string) Region.{region; value=msg} =
  sprintf "%s error in %s:\n%s%!"
    kind (region#to_string `Byte) msg

let prerr ~(kind: string) msg =
  Utils.highlight (format_error ~kind msg)

type file_path = string

let output_token buffer chan token =
  let open Lexing in
  let conc  = Token.to_string token in
  let start = Pos.from_byte buffer.lex_start_p
  and stop  = Pos.from_byte buffer.lex_curr_p in
  Printf.fprintf chan "%s-%s: %s\n%!"
   (start#compact `Byte) (stop#compact `Byte) conc

let iter action file_opt =
  try
    let cin, reset =
      match file_opt with
        None | Some "-" -> stdin, ignore
      |       Some file -> open_in file, reset_file ~file in
    let buffer = Lexing.from_channel cin in
    let rec iter () =
      try
        let t = scan buffer in
        action buffer stdout t;
        if t = Token.EOF then (close_in cin; close_out stdout)
        else iter ()
      with Error diag ->
             close_in cin; close_out stdout;
             prerr ~kind:"Lexical" diag
    in reset buffer; iter ()
  with Sys_error msg -> Utils.highlight msg

let trace = iter output_token
(* END TRAILER *)
}
