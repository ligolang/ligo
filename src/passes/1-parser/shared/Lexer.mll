(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
[@@@warning "-42"]

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

(* START HEADER *)

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. Consequently, generic functions to
   construct tokens are provided. Note predicate [is_eof], which
   caracterises the virtual token for end-of-file, because it requires
   special handling. *)

type lexeme = string

module type TOKEN =
  sig
    type token

    (* Errors *)

    type   int_err = Non_canonical_zero
    type ident_err = Reserved_name
    type   nat_err = Invalid_natural
                   | Non_canonical_zero_nat
    type   sym_err = Invalid_symbol
    type  attr_err = Invalid_attribute

    (* Injections *)

    val mk_int      : lexeme -> Region.t -> (token,   int_err) result
    val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
    val mk_mutez    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident    : lexeme -> Region.t -> (token, ident_err) result
    val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
    val mk_string   : lexeme -> Region.t -> token
    val mk_verbatim : lexeme -> Region.t -> token
    val mk_bytes    : lexeme -> Region.t -> token
    val mk_constr   : lexeme -> Region.t -> token
    val mk_attr     : string -> lexeme -> Region.t -> (token, attr_err) result
    val eof         : Region.t -> token

    (* Predicates *)

    val is_eof    : token -> bool

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
    val to_region : token -> Region.t

    (* Style *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg

    val check_right_context :
      token ->
      (Lexing.lexbuf -> (Markup.t list * token) option) ->
      Lexing.lexbuf ->
      unit
  end


(* The functorised interface

   Note that the module parameter [Token] is re-exported as a
   submodule in [S].
 *)

module type S =
  sig
    module Token : TOKEN
    type token = Token.token

    val scan :
      token LexerLib.state -> Lexing.lexbuf -> token LexerLib.state

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg
  end

module Make (Token : TOKEN) : (S with module Token = Token) =
  struct
    module Token = Token
    type token = Token.token

    (* ERRORS *)

    type error =
      Invalid_utf8_sequence
    | Unexpected_character of char
    | Undefined_escape_sequence
    | Unterminated_string
    | Unterminated_verbatim
    | Unterminated_comment of string
    | Non_canonical_zero
    | Broken_string
    | Invalid_character_in_string
    | Reserved_name of string
    | Invalid_symbol
    | Invalid_natural
    | Invalid_attribute

    let sprintf = Printf.sprintf

    let error_to_string = function
      Invalid_utf8_sequence ->
        "Invalid UTF-8 sequence."
    | Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Undefined_escape_sequence ->
        "Undefined escape sequence.\n\
         Hint: Remove or replace the sequence."
    | Unterminated_string ->
        "Unterminated string.\n\
         Hint: Close with double quotes."
    | Unterminated_verbatim ->
        "Unterminated verbatim.\n\
         Hint: Close with \"|}\"."
    | Unterminated_comment ending ->
        sprintf "Unterminated comment.\n\
                 Hint: Close with \"%s\"." ending
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Broken_string ->
        "The string starting here is interrupted by a line break.\n\
         Hint: Remove the break, close the string before or insert a \
         backslash."
    | Invalid_character_in_string ->
        "Invalid character in string.\n\
         Hint: Remove or replace the character."
    | Reserved_name s ->
        sprintf "Reserved name: \"%s\".\n\
         Hint: Change the name." s
    | Invalid_symbol ->
        "Invalid symbol.\n\
         Hint: Check the LIGO syntax you use."
    | Invalid_natural ->
        "Invalid natural number."
    | Invalid_attribute ->
        "Invalid attribute."

    exception Error of error Region.reg

    let format_error ?(offsets=true) mode Region.{region; value} ~file =
      let msg = error_to_string value
      and reg = region#to_string ~file ~offsets mode in
      let value = sprintf "Lexical error %s:\n%s\n" reg msg
      in Region.{value; region}

    let fail region value = raise (Error Region.{region; value})

    (* TOKENS *)

    (* Making tokens *)

    let mk_string (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_string lexeme region
      in state#enqueue token

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in state#enqueue token

    let mk_bytes bytes state buffer =
      let region, _, state = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in state#enqueue token

    let mk_int state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural

    let mk_mutez state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_mutez lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_tez state buffer =
      let region, lexeme, state = state#sync buffer in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      let lexeme = Z.mul (Z.of_int 1_000_000) (Z.of_string lexeme) in
      match Token.mk_mutez (Z.to_string lexeme ^ "mutez") region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let format_tez s =
      match String.index s '.' with
        index ->
          let len         = String.length s in
          let integral    = Str.first_chars s index
          and fractional  = Str.last_chars s (len-index-1) in
          let num         = Z.of_string (integral ^ fractional)
          and den         = Z.of_string ("1" ^ String.make (len-index-1) '0')
          and million     = Q.of_string "1000000" in
          let mutez       = Q.make num den |> Q.mul million in
          let should_be_1 = Q.den mutez in
          if Z.equal Z.one should_be_1 then Some (Q.num mutez) else None
      | exception Not_found -> assert false

    let mk_tez_decimal state buffer =
      let region, lexeme, state = state#sync buffer in
      let lexeme = Str.(global_replace (regexp "_") "" lexeme) in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      match format_tez lexeme with
        None -> assert false
      | Some tz ->
          match Token.mk_mutez (Z.to_string tz ^ "mutez") region with
            Ok token -> state#enqueue token
        | Error Token.Non_canonical_zero ->
            fail region Non_canonical_zero

    let mk_ident state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_ident lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Reserved_name -> fail region (Reserved_name lexeme)

    let mk_attr header attr state buffer =
      let region, _, state = state#sync buffer in
      match Token.mk_attr header attr region with
        Ok token -> state#enqueue token
      | Error Token.Invalid_attribute ->
          fail region Invalid_attribute

    let mk_constr state buffer =
      let region, lexeme, state = state#sync buffer in
      let token = Token.mk_constr lexeme region
      in state#enqueue token

    let mk_sym state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Invalid_symbol -> fail region Invalid_symbol

    let mk_eof state buffer =
      let region, _, state = state#sync buffer in
      let token = Token.eof region
      in state#enqueue token

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let utf8_bom   = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let decimal    = natural '.' natural
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let letter     = small | capital
let ident      = small (letter | '_' | digit)*
let constr     = capital (letter | '_' | digit)*
let attr       = ident | constr
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as seq)
let esc        = "\\n" | "\\\"" | "\\\\" | "\\b"
               | "\\r" | "\\t" | "\\x" byte

(* Symbols *)

let common_sym     =   ';' | ',' | '(' | ')'  | '[' | ']'  | '{' | '}'
                     | '=' | ':' | '|' | "->" | '.' | '_'  | '^'
                     | '+' | '-' | '*' | '/'  | '<' | "<=" | '>' | ">="
let pascaligo_sym  = "=/=" | '#' | ":="
let cameligo_sym   = "<>" | "::" | "||" | "&&"
let reasonligo_sym = '!' | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"

let symbol = common_sym | pascaligo_sym | cameligo_sym | reasonligo_sym

(* Comments *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment           = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment            = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment

(* #include files *)

let string = [^'"' '\\' '\n']*  (* For strings of #include *)

(* RULES *)

(* Except for the first rule [init], all rules bear a name starting
   with "scan".

   All have a parameter [state] that they thread through their
   recursive calls. The rules for the structured constructs (strings
   and comments) have an extra parameter of type [thread] to record
   the location where they start, and their contents (see above).
 *)

rule init state = parse
  utf8_bom { scan (state#push_bom lexbuf) lexbuf          }
| _        { LexerLib.rollback lexbuf; scan state lexbuf  }

and scan state = parse
  nl                     { scan (state#push_newline lexbuf) lexbuf }
| ' '+                   { scan (state#push_space   lexbuf) lexbuf }
| '\t'+                  { scan (state#push_tabs    lexbuf) lexbuf }

| ident                  { mk_ident        state lexbuf }
| constr                 { mk_constr       state lexbuf }
| bytes                  { mk_bytes seq    state lexbuf }
| natural 'n'            { mk_nat          state lexbuf }
| natural "mutez"        { mk_mutez        state lexbuf }
| natural "tz"
| natural "tez"          { mk_tez          state lexbuf }
| decimal "tz"
| decimal "tez"          { mk_tez_decimal  state lexbuf }
| natural                { mk_int          state lexbuf }
| symbol                 { mk_sym          state lexbuf }
| eof                    { mk_eof          state lexbuf }
| "[@"  (attr as a) "]"  { mk_attr "[@"  a state lexbuf }
| "[@@" (attr as a) "]"  { mk_attr "[@@" a state lexbuf }

  (* Management of #include preprocessing directives

    An input LIGO program may contain preprocessing directives, and
    the entry modules (named *Main.ml) run the preprocessor on them,
    as if using the GNU C preprocessor in traditional mode:

    https://gcc.gnu.org/onlinedocs/cpp/Traditional-Mode.html

      The main interest in using a preprocessor is that it can stand
    for a poor man's (flat) module system for LIGO thanks to #include
    directives, and the equivalent of the traditional mode leaves the
    markup undisturbed.

      Contrary to the C preprocessor, our preprocessor does not
    generate #line resulting from processing #include directives deal
    with system file headers and thus have to be ignored for our
    purpose. Moreover, these #line directives may also carry some
    additional flags:

    https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

    of which 1 and 2 indicate, respectively, the start of a new file
    and the return from a file (after its inclusion has been
    processed).
   *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"' {
    let  _, _, state = state#sync lexbuf in
    let flags, state = scan_flags state [] lexbuf in
    let           () = ignore flags in
    let line         = int_of_string line
    and file         = Filename.basename file in
    let pos          = state#pos#set ~file ~line ~offset:0 in
    let state        = state#set_pos pos in
    scan state lexbuf }

(* String *)

| '"' { let opening, _, state = state#sync lexbuf in
        let thread = LexerLib.mk_thread opening "" in
        scan_string thread state lexbuf |> mk_string }

| "{|" { let opening, _, state = state#sync lexbuf in
        let thread = LexerLib.mk_thread opening "" in
        scan_verbatim thread state lexbuf |> mk_verbatim }

(* Comments *)

| block_comment_openings {
    let lexeme = Lexing.lexeme lexbuf in
    match state#block with
      Some block when block#opening = lexeme ->
        let opening, _, state = state#sync lexbuf in
        let thread            = LexerLib.mk_thread opening lexeme in
        let thread, state     = scan_block block thread state lexbuf in
        let state             = state#push_block thread
        in scan state lexbuf
    | Some _ | None ->
        let n = String.length lexeme in
          begin
            LexerLib.rollback lexbuf;
            assert (n > 0);
            scan (scan_n_sym n state lexbuf) lexbuf
          end }

| line_comments {
    let lexeme = Lexing.lexeme lexbuf in
    match state#line with
      Some line when line = lexeme ->
        let opening, _, state = state#sync lexbuf in
        let thread            = LexerLib.mk_thread opening lexeme in
        let thread, state     = scan_line thread state lexbuf in
        let state             = state#push_line thread
        in scan state lexbuf
    | Some _ | None ->
        let n = String.length lexeme in
          begin
            LexerLib.rollback lexbuf;
            scan (scan_n_sym n state lexbuf) lexbuf
          end }

| _ as c { let region, _, _ = state#sync lexbuf
           in fail region (Unexpected_character c) }

(* Scanning a series of symbols *)

and scan_n_sym n state = parse
  symbol { let state = mk_sym state lexbuf in
           if n = 1 then state
           else scan_n_sym (n-1) state lexbuf }

(* Scanning #include flag *)

and scan_flags state acc = parse
  blank+          { let _, _, state = state#sync lexbuf
                    in scan_flags state acc lexbuf          }
| natural as code { let _, _, state = state#sync lexbuf in
                    let acc = int_of_string code :: acc
                    in scan_flags state acc lexbuf          }
| nl              { List.rev acc, state#push_newline lexbuf }
| eof             { let _, _, state = state#sync lexbuf
                    in List.rev acc, state                  }

(* Finishing a string *)

and scan_string thread state = parse
  nl     { fail thread#opening Broken_string }
| eof    { fail thread#opening Unterminated_string }
| ['\t' '\r' '\b']
         { let region, _, _ = state#sync lexbuf
           in fail region Invalid_character_in_string }
| '"'    { let _, _, state = state#sync lexbuf
           in thread, state }
| esc    { let _, lexeme, state = state#sync lexbuf in
           let thread = thread#push_string lexeme
           in scan_string thread state lexbuf }
| '\\' _ { let region, _, _ = state#sync lexbuf
           in fail region Undefined_escape_sequence }
| _ as c { let _, _, state = state#sync lexbuf in
           scan_string (thread#push_char c) state lexbuf }

and scan_verbatim thread state = parse
| eof        { fail thread#opening Unterminated_verbatim}
| "|}"       { let _, _, state = state#sync lexbuf 
               in thread, state }
| _ as c     { let _, _, state = state#sync lexbuf in
               scan_verbatim (thread#push_char c) state lexbuf }

(* Finishing a block comment

   (For Emacs: ("(*") The lexing of block comments must take care of
   embedded block comments that may occur within, as well as strings,
   so no substring "*/" or "*)" may inadvertently close the
   block. This is the purpose of the first case of the scanner
   [scan_block].
*)

and scan_block block thread state = parse
  '"' | block_comment_openings {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#opening = lexeme || lexeme = "\""
    then let opening            = thread#opening in
         let opening', _, state = state#sync lexbuf in
         let thread             = thread#push_string lexeme in
         let thread             = thread#set_opening opening' in
         let next               = if lexeme = "\"" then scan_string
                                  else scan_block block in
         let thread, state      = next thread state lexbuf in
         let thread             = thread#set_opening opening
         in scan_block block thread state lexbuf
    else let ()    = LexerLib.rollback lexbuf in
         let n     = String.length lexeme in
         let ()    = assert (n > 0) in
         let state = scan_n_sym n state lexbuf
         in scan_block block thread state lexbuf }

| block_comment_closings {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#closing = lexeme
    then let _, _, state = state#sync lexbuf
         in thread#push_string lexeme, state
    else let ()    = LexerLib.rollback lexbuf in
         let n     = String.length lexeme in
         let ()    = assert (n > 0) in
         let state = scan_n_sym n state lexbuf
         in scan_block block thread state lexbuf }

| nl as nl {
    let ()     = Lexing.new_line lexbuf
    and state  = state#set_pos (state#pos#new_line nl)
    and thread = thread#push_string nl in
    scan_block block thread state lexbuf }

| eof { let err = Unterminated_comment (block#closing)
        in fail thread#opening err }

| _   { let ()             = LexerLib.rollback lexbuf in
        let len            = thread#length in
        let thread, status = scan_utf8 block thread state lexbuf in
        let delta          = thread#length - len in
        let pos            = state#pos#shift_one_uchar delta in
        match status with
          Stdlib.Ok () ->
            scan_block block thread (state#set_pos pos) lexbuf
        | Error error ->
            let region = Region.make ~start:state#pos ~stop:pos
            in fail region error }

(* Finishing a line comment *)

and scan_line thread state = parse
  nl as nl { let     () = Lexing.new_line lexbuf
             and thread = thread#push_string nl
             and state  = state#set_pos (state#pos#new_line nl)
             in thread, state }
| eof      { thread, state }
| _        { let     () = LexerLib.rollback lexbuf in
             let len    = thread#length in
             let thread,
                 status = scan_utf8_inline thread state lexbuf in
             let delta  = thread#length - len in
             let pos    = state#pos#shift_one_uchar delta in
             match status with
               Stdlib.Ok () ->
                 scan_line thread (state#set_pos pos) lexbuf
             | Error error ->
                 let region = Region.make ~start:state#pos ~stop:pos
                 in fail region error }

and scan_utf8 block thread state = parse
     eof { let err = Unterminated_comment block#closing
           in fail thread#opening err }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _ -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8 block thread state lexbuf
           | `End         -> assert false }

and scan_utf8_inline thread state = parse
     eof { thread, Stdlib.Ok () }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _ -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8_inline thread state lexbuf
           | `End         -> assert false }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let scan =
  let first_call = ref true in
  fun state lexbuf ->
    if   !first_call
    then (first_call := false; init state lexbuf)
    else scan state lexbuf

end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
