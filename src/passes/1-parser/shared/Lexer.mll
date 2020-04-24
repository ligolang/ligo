(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
[@@@warning "-42"]

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

(* START HEADER *)

type lexeme = string

(* ALIASES *)

let sprintf = Printf.sprintf

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. Consequently, generic functions to
   construct tokens are provided. Note predicate [is_eof], which
   caracterises the virtual token for end-of-file, because it requires
   special handling. *)

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

    val mk_int    : lexeme -> Region.t -> (token,   int_err) result
    val mk_nat    : lexeme -> Region.t -> (token,   nat_err) result
    val mk_mutez  : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
    val mk_sym    : lexeme -> Region.t -> (token,   sym_err) result
    val mk_string : lexeme -> Region.t -> token
    val mk_bytes  : lexeme -> Region.t -> token
    val mk_constr : lexeme -> Region.t -> token
    val mk_attr   : string -> lexeme -> Region.t -> (token, attr_err) result
    val eof       : Region.t -> token

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

module Make (Token: TOKEN) : (LexerLib.S with module Token = Token) =
  struct
    module Token = Token
    type token = Token.token

    type file_path = string

    type line_comment = LexerLib.line_comment
    type block_comment = LexerLib.block_comment
    let mk_block = LexerLib.mk_block

    (* ERRORS *)

    type error =
      Invalid_utf8_sequence
    | Unexpected_character of char
    | Undefined_escape_sequence
    (*    | Missing_break*)
    | Unterminated_string
    (*    | Unterminated_integer*)
    (*    | Odd_lengthed_bytes*)
    | Unterminated_comment of string
    (*    | Orphan_minus*)
    | Non_canonical_zero
    (*    | Negative_byte_sequence *)
    | Broken_string
    | Invalid_character_in_string
    | Reserved_name of string
    | Invalid_symbol
    | Invalid_natural
    | Invalid_attribute

    let error_to_string = function
      Invalid_utf8_sequence ->
        "Invalid UTF-8 sequence."
    | Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Undefined_escape_sequence ->
        "Undefined escape sequence.\n\
         Hint: Remove or replace the sequence."
(*    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space."
 *)    | Unterminated_string ->
        "Unterminated string.\n\
         Hint: Close with double quotes."
(*    | Unterminated_integer ->
        "Unterminated integer.\n\
         Hint: Remove the sign or proceed with a natural number." *)
(*    | Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit."
 *)    | Unterminated_comment ending ->
        sprintf "Unterminated comment.\n\
                 Hint: Close with \"%s\"." ending
(*    | Orphan_minus ->
        "Orphan minus sign.\n\
         Hint: Remove the trailing space." *)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
(*    | Negative_byte_sequence ->
        "Negative byte sequence.\n\
         Hint: Remove the leading minus sign." *)
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

let pascaligo_block_comment_opening = "(*"
let pascaligo_block_comment_closing = "*)"
let pascaligo_line_comment          = "//"

let cameligo_block_comment_opening = "(*"
let cameligo_block_comment_closing = "*)"
let cameligo_line_comment          = "//"

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

(* Some special errors *)

(*
| '-' { let region, _, state = state#sync lexbuf in
        let state = scan state lexbuf in
        let open Markup in
        match FQueue.peek state#units with
          None -> assert false
        | Some (_, ((Space _ | Tabs _)::_, token))
            when Token.is_int token -> fail region Orphan_minus
        | _ -> fail region Unterminated_integer }

| "-0x" byte_seq?
      { let region, _, _ = state#sync lexbuf
        in fail region Negative_byte_sequence }
 *)

| '"' { let opening, lexeme, state = state#sync lexbuf in
        let thread = LexerLib.mk_thread opening lexeme in
        scan_string thread state lexbuf |> mk_string }

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

(* TODO: Move below to [LexerCommon.mll] *)

(* Finishing a string *)

and scan_string thread state = parse
  nl     { fail thread#opening Broken_string }
| eof    { fail thread#opening Unterminated_string }
| ['\t' '\r' '\b']
         { let region, _, _ = state#sync lexbuf
           in fail region Invalid_character_in_string }
| '"'    { let _, _, state = state#sync lexbuf
           in thread#push_char '"', state }
| esc    { let _, lexeme, state = state#sync lexbuf in
           let thread = thread#push_string lexeme
           in scan_string thread state lexbuf }
| '\\' _ { let region, _, _ = state#sync lexbuf
           in fail region Undefined_escape_sequence }
| _ as c { let _, _, state = state#sync lexbuf in
           scan_string (thread#push_char c) state lexbuf }

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


(* Scanning the lexing buffer for tokens (and markup, as a
   side-effect).

     Because we want the lexer to have access to the right lexical
   context of a recognised lexeme (to enforce stylistic constraints or
   report special error patterns), we need to keep a hidden reference
   to a queue of recognised lexical units (that is, tokens and markup)
   that acts as a mutable state between the calls to [read]. When
   [read] is called, that queue is examined first and, if it contains
   at least one token, that token is returned; otherwise, the lexing
   buffer is scanned for at least one more new token. That is the
   general principle: we put a high-level buffer (our queue) on top of
   the low-level lexing buffer.

     One tricky and important detail is that we must make any parser
   generated by Menhir (and calling [read]) believe that the last
   region of the input source that was matched indeed corresponds to
   the returned token, despite that many tokens and markup may have
   been matched since it was actually read from the input. In other
   words, the parser requests a token that is taken from the
   high-level buffer, but the parser requests the source regions from
   the _low-level_ lexing buffer, and they may disagree if more than
   one token has actually been recognised.

     Consequently, in order to maintain a consistent view for the
   parser, we have to patch some fields of the lexing buffer, namely
   [lex_start_p] and [lex_curr_p], as these fields are read by parsers
   generated by Menhir when querying source positions (regions). This
   is the purpose of the function [patch_buffer]. After reading one
   or more tokens and markup by the scanning rule [scan], we have to
   save in the hidden reference [buf_reg] the region of the source
   that was matched by [scan]. This atomic sequence of patching,
   scanning and saving is implemented by the _function_ [scan]
   (beware: it shadows the scanning rule [scan]). The function
   [patch_buffer] is, of course, also called just before returning the
   token, so the parser has a view of the lexing buffer consistent
   with the token.

     Note that an additional reference [first_call] is needed to
   distinguish the first call to the function [scan], as the first
   scanning rule is actually [init] (which can handle the BOM), not
   [scan].
 *)

type logger = Markup.t list -> token -> unit

type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type instance = {
  input    : input;
  read     : log:logger -> Lexing.lexbuf -> token;
  buffer   : Lexing.lexbuf;
  get_win  : unit -> token LexerLib.window;
  get_pos  : unit -> Pos.t;
  get_last : unit -> Region.t;
  get_file : unit -> file_path;
  close    : unit -> unit
}

type open_err = File_opening of string

let lexbuf_from_input = function
  File path ->
   (try
      let chan = open_in path in
      let close () = close_in chan in
      let lexbuf = Lexing.from_channel chan in
      let () =
        let open Lexing in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=path}
      in Ok (lexbuf, close)
    with Sys_error msg -> Stdlib.Error (File_opening msg))
| String s ->
    Ok (Lexing.from_string s, fun () -> ())
| Channel chan ->
    let close () = close_in chan in
    Ok (Lexing.from_channel chan, close)
| Buffer b -> Ok (b, fun () -> ())

let open_token_stream ?line ?block input =
  let file_path  = match input with
                     File path -> path
                   | _ -> "" in
  let        pos = Pos.min ~file:file_path in
  let    buf_reg = ref (pos#byte, pos#byte)
  and first_call = ref true
  and    decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let     supply = Uutf.Manual.src decoder in
  let      state = ref (LexerLib.mk_state
                          ~units:FQueue.empty
                          ~last:Region.ghost
                          ~window:LexerLib.Nil
                          ~pos
                          ~markup:[]
                          ~decoder
                          ~supply
                          ?block
                          ?line
                          ()) in

  let get_pos  () = !state#pos
  and get_last () = !state#last
  and get_win  () = !state#window
  and get_file () = file_path in

  let patch_buffer (start, stop) buffer =
    let open Lexing in
    let file_path = buffer.lex_curr_p.pos_fname in
    buffer.lex_start_p <- {start with pos_fname = file_path};
    buffer.lex_curr_p  <- {stop  with pos_fname = file_path}

  and save_region buffer =
    buf_reg := Lexing.(buffer.lex_start_p, buffer.lex_curr_p) in

  let scan buffer =
    patch_buffer !buf_reg buffer;
    (if   !first_call
     then (state := init !state buffer; first_call := false)
     else state := scan !state buffer);
    save_region buffer in

  let next_token buffer =
    scan buffer;
    match FQueue.peek !state#units with
                         None -> None
    | Some (units, ext_token) ->
        state := !state#set_units units; Some ext_token in

  let rec read ~log buffer =
    match FQueue.deq !state#units with
      None ->
        scan buffer;
        read ~log buffer
    | Some (units, (left_mark, token)) ->
        log left_mark token;
        state := ((!state#set_units units)
                  #set_last (Token.to_region token))
                  #slide_token token;
        Token.check_right_context token next_token buffer;
        patch_buffer (Token.to_region token)#byte_pos buffer;
        token in
  match lexbuf_from_input input with
    Ok (buffer, close) ->
      let () =
        match input with
          File path when path <> "" -> LexerLib.reset ~file:path buffer
        | _ -> () in
      let instance = {
        input; read; buffer; get_win; get_pos; get_last; get_file; close}
      in Ok instance
  | Error _ as e -> e

end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
