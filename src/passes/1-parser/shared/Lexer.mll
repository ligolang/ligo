(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
[@@@warning "-42"]

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

(* START HEADER *)

type lexeme = string

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

(* LEXER ENGINE *)

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line buffer] modifies in-place the lexing
   buffer [buffer] so the lexing engine records that the file
   associated with [buffer] is named [file], and the current line is
   [line]. This function is useful when lexing a file that has been
   previously preprocessed by the C preprocessor, in which case the
   argument [file] is the name of the file that was preprocessed,
   _not_ the preprocessed file (of which the user is not normally
   aware). By default, the [line] argument is [1].
*)

let reset_file ~file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line ~line buffer =
  assert (line >= 0);
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

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

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

    val is_string : token -> bool
    val is_bytes  : token -> bool
    val is_int    : token -> bool
    val is_ident  : token -> bool
    val is_kwd    : token -> bool
    val is_constr : token -> bool
    val is_sym    : token -> bool
    val is_eof    : token -> bool

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
    val to_region : token -> Region.t
  end

(* The module type for lexers is [S]. *)

module type S =
  sig
    module Token : TOKEN
    type token = Token.token

    type file_path = string
    type logger = Markup.t list -> token -> unit

    type window =
      Nil
    | One of token
    | Two of token * token

    val slide : token -> window -> window

    type instance = {
      read     : log:logger -> Lexing.lexbuf -> token;
      buffer   : Lexing.lexbuf;
      get_win  : unit -> window;
      get_pos  : unit -> Pos.t;
      get_last : unit -> Region.t;
      get_file : unit -> file_path;
      close    : unit -> unit
    }

    type input =
      File    of file_path (* "-" means stdin *)
    | Stdin
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type open_err = File_opening of string

    val open_token_stream : input -> (instance, open_err) Stdlib.result

    (* Error reporting *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg
  end

(* The functorised interface

   Note that the module parameter [Token] is re-exported as a
   submodule in [S].
 *)

module Make (Token: TOKEN) : (S with module Token = Token) =
  struct
    module Token = Token
    type token = Token.token

    type file_path = string

    (* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

    (* When scanning structured constructs, like strings and comments,
       we need to keep the region of the opening symbol (like double
       quote, "//" or "(*") in order to report any error more
       precisely. Since ocamllex is byte-oriented, we need to store
       the parsed bytes as characters in an accumulator [acc] and
       also its length [len], so, we are done, it is easy to build the
       string making up the structured construct with [mk_str] (see
       above).

       The resulting data structure is called a _thread_.
       (Note for Emacs: "*)".)
    *)

    type thread = {
      opening : Region.t;
      len     : int;
      acc     : char list
    }

    let push_char char {opening; len; acc} = {opening; len=len+1; acc=char::acc}

    let push_string str {opening; len; acc} =
      {opening;
       len = len + String.length str;
       acc = explode str acc}

    (* STATE *)

    (** The type [buffer] models a two-slot buffer of tokens for
       reporting after a parse error.

       In [Two(t1,t2)], the token [t2] is the next to be sent to the
       parser.

       The call [slide token buffer] pushes the token [token] in the
       buffer [buffer]. If the buffer is full, that is, it is
       [Two(t1,t2)], then the token [t2] is discarded to make room for
       [token].
     *)
    type window =
      Nil
    | One of token
    | Two of token * token

    let slide token = function
                    Nil -> One token
    | One t | Two (t,_) -> Two (token,t)

    (** Beyond tokens, the result of lexing is a state. The type
       [state] represents the logical state of the lexing engine, that
       is, a value which is threaded during scanning and which denotes
       useful, high-level information beyond what the type
       [Lexing.lexbuf] in the standard library already provides for
       all generic lexers.

         Tokens are the smallest units used by the parser to build the
       abstract syntax tree. The state includes a queue of recognised
       tokens, with the markup at the left of its lexeme until either
       the start of the file or the end of the previously recognised
       token.

         The markup from the last recognised token or, if the first
       token has not been recognised yet, from the beginning of the
       file is stored in the field [markup] of the state because it is
       a side-effect, with respect to the output token list, and we
       use a record with a single field [units] because that record
       may be easily extended during the future maintenance of this
       lexer.

         The state also includes a field [pos] which holds the current
       position in the LIGO source file. The position is not always
       updated after a single character has been matched: that depends
       on the regular expression that matched the lexing buffer.

         The field [win] is a two-token window, that is, a buffer that
       contains the last recognised token, and the penultimate (if
       any).

         The fields [decoder] and [supply] offer the support needed
       for the lexing of UTF-8 encoded characters in comments (the
       only place where they are allowed in LIGO). The former is the
       decoder proper and the latter is the effectful function
       [supply] that takes a byte, a start index and a length and feed
       it to [decoder]. See the documentation of the third-party
       library Uutf.
     *)
    type state = {
      units   : (Markup.t list * token) FQueue.t;
      markup  : Markup.t list;
      window  : window;
      last    : Region.t;
      pos     : Pos.t;
      decoder : Uutf.decoder;
      supply  : Bytes.t -> int -> int -> unit
    }

    (* The call [enqueue (token, state)] updates functionally the
       state [state] by associating the token [token] with the stored
       markup and enqueuing the pair into the units queue. The field
       [markup] is then reset to the empty list. *)

    let enqueue (token, state) = {
      state with
        units  = FQueue.enq (state.markup, token) state.units;
        markup = []
    }

    (* The call [sync state buffer] updates the current position in
       accordance with the contents of the lexing buffer, more
       precisely, depending on the length of the string which has just
       been recognised by the scanner: that length is used as a
       positive offset to the current column. *)

    let sync state buffer =
      let lex   = Lexing.lexeme buffer in
      let len   = String.length lex in
      let start = state.pos in
      let stop  = start#shift_bytes len in
      let state = {state with pos = stop}
      in Region.make ~start ~stop, lex, state

    (* MARKUP *)

    (* Committing markup to the current logical state *)

    let push_newline state buffer =
      let value  = Lexing.lexeme buffer
      and ()     = Lexing.new_line buffer
      and start  = state.pos in
      let stop   = start#new_line value in
      let state  = {state with pos = stop}
      and region = Region.make ~start ~stop in
      let unit   = Markup.Newline Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    let push_line (thread, state) =
      let start  = thread.opening#start in
      let region = Region.make ~start ~stop:state.pos
      and value  = mk_str thread.len thread.acc in
      let unit   = Markup.LineCom Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    let push_block (thread, state) =
      let start  = thread.opening#start in
      let region = Region.make ~start ~stop:state.pos
      and value  = mk_str thread.len thread.acc in
      let unit   = Markup.BlockCom Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    let push_space state buffer =
      let region, lex, state = sync state buffer in
      let value  = String.length lex in
      let unit   = Markup.Space Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    let push_tabs state buffer =
      let region, lex, state = sync state buffer in
      let value  = String.length lex in
      let unit   = Markup.Tabs Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    let push_bom state buffer =
      let region, value, state = sync state buffer in
      let unit   = Markup.BOM Region.{region; value} in
      let markup = unit :: state.markup
      in {state with markup}

    (* ERRORS *)

    type error =
      Invalid_utf8_sequence
    | Unexpected_character of char
    | Undefined_escape_sequence
    | Missing_break
    | Unterminated_string
    | Unterminated_integer
    | Odd_lengthed_bytes
    | Unterminated_comment
    | Orphan_minus
    | Non_canonical_zero
    | Negative_byte_sequence
    | Broken_string
    | Invalid_character_in_string
    | Reserved_name of string
    | Invalid_symbol
    | Invalid_natural
    | Invalid_attribute

    let error_to_string = function
      Invalid_utf8_sequence ->
        "Invalid UTF-8 sequence.\n"
    | Unexpected_character c ->
        sprintf "Unexpected character '%s'.\n" (Char.escaped c)
    | Undefined_escape_sequence ->
        "Undefined escape sequence.\n\
         Hint: Remove or replace the sequence.\n"
    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space.\n"
    | Unterminated_string ->
        "Unterminated string.\n\
         Hint: Close with double quotes.\n"
    | Unterminated_integer ->
        "Unterminated integer.\n\
         Hint: Remove the sign or proceed with a natural number.\n"
    | Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit.\n"
    | Unterminated_comment ->
        "Unterminated comment.\n\
         Hint: Close with \"*)\".\n"
    | Orphan_minus ->
        "Orphan minus sign.\n\
         Hint: Remove the trailing space.\n"
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0.\n"
    | Negative_byte_sequence ->
        "Negative byte sequence.\n\
         Hint: Remove the leading minus sign.\n"
    | Broken_string ->
        "The string starting here is interrupted by a line break.\n\
         Hint: Remove the break, close the string before or insert a \
         backslash.\n"
    | Invalid_character_in_string ->
        "Invalid character in string.\n\
         Hint: Remove or replace the character.\n"
    | Reserved_name s ->
        sprintf "Reserved name: \"%s\".\n\
         Hint: Change the name.\n" s
    | Invalid_symbol ->
        "Invalid symbol.\n\
         Hint: Check the LIGO syntax you use.\n"
    | Invalid_natural ->
        "Invalid natural."
    | Invalid_attribute ->
        "Invalid attribute."

    exception Error of error Region.reg

    let format_error ?(offsets=true) mode Region.{region; value} ~file =
      let msg = error_to_string value
      and reg = region#to_string ~file ~offsets mode in
      let value = sprintf "Lexical error %s:\n%s" reg msg
      in Region.{value; region}

    let fail region value = raise (Error Region.{region; value})

    (* TOKENS *)

    (* Making tokens *)

    let mk_string (thread, state) =
      let start  = thread.opening#start in
      let stop   = state.pos in
      let region = Region.make ~start ~stop in
      let lexeme = mk_str thread.len thread.acc in
      let token  = Token.mk_string lexeme region
      in token, state

    let mk_bytes bytes state buffer =
      let region, _, state = sync state buffer in
      let token = Token.mk_bytes bytes region
      in token, state

    let mk_int state buffer =
      let region, lexeme, state = sync state buffer in
      match Token.mk_int lexeme region with
        Ok token -> token, state
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let region, lexeme, state = sync state buffer in
      match Token.mk_nat lexeme region with
        Ok token -> token, state
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural

    let mk_mutez state buffer =
      let region, lexeme, state = sync state buffer in
      match Token.mk_mutez lexeme region with
        Ok token -> token, state
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_tez state buffer =
      let region, lexeme, state = sync state buffer in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      let lexeme = Z.mul (Z.of_int 1_000_000) (Z.of_string lexeme) in
      match Token.mk_mutez (Z.to_string lexeme ^ "mutez") region with
        Ok token -> token, state
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
      let region, lexeme, state = sync state buffer in
      let lexeme = Str.(global_replace (regexp "_") "" lexeme) in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      match format_tez lexeme with
        None -> assert false
      | Some tz ->
          match Token.mk_mutez (Z.to_string tz ^ "mutez") region with
            Ok token -> token, state
        | Error Token.Non_canonical_zero ->
            fail region Non_canonical_zero

    let mk_ident state buffer =
      let region, lexeme, state = sync state buffer in
      match Token.mk_ident lexeme region with
        Ok token -> token, state
      | Error Token.Reserved_name -> fail region (Reserved_name lexeme)

    let mk_attr header attr state buffer =
      let region, _, state = sync state buffer in
      match Token.mk_attr header attr region with
        Ok token ->
          token, state
      | Error Token.Invalid_attribute ->
          fail region Invalid_attribute

    let mk_constr state buffer =
      let region, lexeme, state = sync state buffer
      in Token.mk_constr lexeme region, state

    let mk_sym state buffer =
      let region, lexeme, state = sync state buffer in
      match Token.mk_sym lexeme region with
        Ok token -> token, state
      | Error Token.Invalid_symbol -> fail region Invalid_symbol

    let mk_eof state buffer =
      let region, _, state = sync state buffer
      in Token.eof region, state

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

let pascaligo_sym  = "=/=" | '#' | ":="
let cameligo_sym   = "<>" | "::" | "||" | "&&"
let reasonligo_sym = '!' | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"

let symbol =
  ';' | ',' | '(' | ')'| '[' | ']' | '{' | '}'
| '=' | ':' | '|' | "->" | '.' | '_' | '^'
| '+' | '-' | '*' | '/'
| '<' | "<=" | '>' | ">="
| pascaligo_sym | cameligo_sym | reasonligo_sym

let string = [^'"' '\\' '\n']*  (* For strings of #include *)

(* RULES *)

(* Except for the first rule [init], all rules bear a name starting
   with "scan".

   All have a parameter [state] that they thread through their
   recursive calls. The rules for the structured constructs (strings
   and comments) have an extra parameter of type [thread] (see above).
*)

rule init state = parse
  utf8_bom { scan (push_bom state lexbuf) lexbuf }
| _        { rollback lexbuf; scan state lexbuf  }

and scan state = parse
  nl                     { scan (push_newline state lexbuf) lexbuf }
| ' '+                   { scan (push_space   state lexbuf) lexbuf }
| '\t'+                  { scan (push_tabs state lexbuf) lexbuf    }
| ident                  { mk_ident        state lexbuf |> enqueue  }
| constr                 { mk_constr       state lexbuf |> enqueue  }
| bytes                  { mk_bytes seq    state lexbuf |> enqueue  }
| natural 'n'            { mk_nat          state lexbuf |> enqueue  }
| natural "mutez"        { mk_mutez        state lexbuf |> enqueue  }
| natural "tz"
| natural "tez"          { mk_tez           state lexbuf |> enqueue }
| decimal "tz"
| decimal "tez"          { mk_tez_decimal   state lexbuf |> enqueue }
| natural                { mk_int          state lexbuf |> enqueue  }
| symbol                 { mk_sym          state lexbuf |> enqueue  }
| eof                    { mk_eof          state lexbuf |> enqueue  }
| "[@"  (attr as a) "]"  { mk_attr "[@"  a state lexbuf |> enqueue  }
| "[@@" (attr as a) "]"  { mk_attr "[@@" a state lexbuf |> enqueue  }

| '"'  { let opening, _, state = sync state lexbuf in
         let thread = {opening; len=1; acc=['"']} in
         scan_string thread state lexbuf |> mk_string |> enqueue }

| "(*" { let opening, _, state = sync state lexbuf in
         let thread = {opening; len=2; acc=['*';'(']} in
         let state  = scan_block thread state lexbuf |> push_block
         in scan state lexbuf }

| "//" { let opening, _, state = sync state lexbuf in
         let thread = {opening; len=2; acc=['/';'/']} in
         let state  = scan_line thread state lexbuf |> push_line
         in scan state lexbuf }

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

| '#' blank* ("line" blank+)? (natural as line) blank+
    '"' (string as file) '"' {
    let  _, _, state = sync state lexbuf in
    let flags, state = scan_flags state [] lexbuf in
    let           () = ignore flags in
    let line         = int_of_string line
    and file         = Filename.basename file in
    let pos          = state.pos#set ~file ~line ~offset:0 in
    let state        = {state with pos} in
    scan state lexbuf }

  (* Some special errors

     Some special errors are recognised in the semantic actions of the
     following regular expressions. The first error is a minus sign
     separated from the integer it applies to by some markup (space or
     tabs). The second is a minus sign immediately followed by
     anything else than a natural number (matched above) or markup and
     a number (previous error). The third is the strange occurrence of
     an attempt at defining a negative byte sequence. Finally, the
     catch-all rule reports unexpected characters in the buffer (and
     is not so special, after all).
  *)

| '-' { let region, _, state = sync state lexbuf in
        let state = scan state lexbuf in
        let open Markup in
        match FQueue.peek state.units with
          None -> assert false
        | Some (_, ((Space _ | Tabs _)::_, token))
            when Token.is_int token ->
              fail region Orphan_minus
        | _ -> fail region Unterminated_integer }

| "-0x" byte_seq?
      { let region, _, _ = sync state lexbuf
        in fail region Negative_byte_sequence }

| _ as c { let region, _, _ = sync state lexbuf
           in fail region (Unexpected_character c) }

(* Scanning CPP #include flags *)

and scan_flags state acc = parse
  blank+          { let _, _, state = sync state lexbuf
                    in scan_flags state acc lexbuf          }
| natural as code { let _, _, state = sync state lexbuf in
                    let acc = int_of_string code :: acc
                    in scan_flags state acc lexbuf          }
| nl              { List.rev acc, push_newline state lexbuf }
| eof             { let _, _, state = sync state lexbuf
                    in List.rev acc, state       (* TODO *) }

(* Finishing a string *)

and scan_string thread state = parse
  nl         { fail thread.opening Broken_string }
| eof        { fail thread.opening Unterminated_string }
| ['\t' '\r' '\b']
             { let region, _, _ = sync state lexbuf
               in fail region Invalid_character_in_string }
| '"'        { let _, _, state = sync state lexbuf
               in push_char '"' thread, state }
| esc        { let _, lexeme, state = sync state lexbuf
               in scan_string (push_string lexeme thread) state lexbuf }
| '\\' _     { let region, _, _ = sync state lexbuf
               in fail region Undefined_escape_sequence }
| _ as c     { let _, _, state = sync state lexbuf in
               scan_string (push_char c thread) state lexbuf }

(* Finishing a block comment

   (Note for Emacs: ("(*")
   The lexing of block comments must take care of embedded block
   comments that may occur within, as well as strings, so no substring
   "*)" may inadvertently close the block. This is the purpose
   of the first case of the scanner [scan_block].
*)

and scan_block thread state = parse
  '"' | "(*" { let opening = thread.opening in
               let opening', lexeme, state = sync state lexbuf in
               let thread = push_string lexeme thread in
               let thread = {thread with opening=opening'} in
               let next   = if lexeme = "\"" then scan_string
                            else scan_block in
               let thread, state = next thread state lexbuf in
               let thread = {thread with opening}
               in scan_block thread state lexbuf }
| "*)"       { let _, lexeme, state = sync state lexbuf
               in push_string lexeme thread, state }
| nl as nl   { let ()     = Lexing.new_line lexbuf
               and state  = {state with pos = state.pos#new_line nl}
               and thread = push_string nl thread
               in scan_block thread state lexbuf }
| eof        { fail thread.opening Unterminated_comment }
| _          { let ()     = rollback lexbuf in
               let len    = thread.len in
               let thread,
                   status = scan_utf8 thread state lexbuf in
               let delta  = thread.len - len in
               let pos    = state.pos#shift_one_uchar delta in
               match status with
                 None -> scan_block thread {state with pos} lexbuf
               | Some error ->
                   let region = Region.make ~start:state.pos ~stop:pos
                   in fail region error }

(* Finishing a line comment *)

and scan_line thread state = parse
  nl as nl { let     () = Lexing.new_line lexbuf
             and thread = push_string nl thread
             and state  = {state with pos = state.pos#new_line nl}
             in thread, state }
| eof      { thread, state }
| _        { let     () = rollback lexbuf in
             let len    = thread.len in
             let thread,
                 status = scan_utf8 thread state lexbuf in
             let delta  = thread.len - len in
             let pos    = state.pos#shift_one_uchar delta in
             match status with
               None -> scan_line thread {state with pos} lexbuf
             | Some error ->
                 let region = Region.make ~start:state.pos ~stop:pos
                 in fail region error }

and scan_utf8 thread state = parse
     eof { fail thread.opening Unterminated_comment }
| _ as c { let thread = push_char c thread in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state.supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state.decoder with
             `Uchar _     -> thread, None
           | `Malformed _ -> thread, Some Invalid_utf8_sequence
           | `Await       -> scan_utf8 thread state lexbuf
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

type instance = {
  read     : log:logger -> Lexing.lexbuf -> token;
  buffer   : Lexing.lexbuf;
  get_win  : unit -> window;
  get_pos  : unit -> Pos.t;
  get_last : unit -> Region.t;
  get_file : unit -> file_path;
  close    : unit -> unit
}

type input =
  File    of file_path (* "-" means stdin *)
| Stdin
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type open_err = File_opening of string

let open_token_stream input =
  let file_path  = match input with
                     File file_path ->
                       if file_path = "-" then "" else file_path
                   | _ -> "" in
  let        pos = Pos.min ~file:file_path in
  let    buf_reg = ref (pos#byte, pos#byte)
  and first_call = ref true
  and    decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let     supply = Uutf.Manual.src decoder in
  let      state = ref {units = FQueue.empty;
                        last = Region.ghost;
                        window = Nil;
                        pos;
                        markup = [];
                        decoder;
                        supply} in

  let get_pos  () = !state.pos
  and get_last () = !state.last
  and get_win  () = !state.window
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
    match FQueue.peek !state.units with
                         None -> assert false
    | Some (units, ext_token) ->
        state := {!state with units}; Some ext_token in

  let check_right_context token buffer =
    let open Token in
    if is_int token || is_bytes token then
      match next_token buffer with
        Some ([], next) ->
          let pos    = (Token.to_region token)#stop in
          let region = Region.make ~start:pos ~stop:pos in
          if is_int next then
            fail region Odd_lengthed_bytes
          else
          if is_ident next || is_string next || is_bytes next then
            fail region Missing_break
      | _ -> ()
    else
    if is_ident token || is_string token then
      match next_token buffer with
        Some ([], next) ->
          if is_ident next || is_string next
             || is_bytes next || is_int next
          then
            let pos    = (Token.to_region token)#stop in
            let region = Region.make ~start:pos ~stop:pos
            in fail region Missing_break
      | _ -> () in

  let rec read ~log buffer =
    match FQueue.deq !state.units with
      None ->
        scan buffer;
        read ~log buffer
    | Some (units, (left_mark, token)) ->
        log left_mark token;
        state := {!state with units;
                              last = Token.to_region token;
                              window = slide token !state.window};
        check_right_context token buffer;
        patch_buffer (Token.to_region token)#byte_pos buffer;
        token in

  let buf_close_res =
    match input with
      File "" | File "-" | Stdin ->
        Ok (Lexing.from_channel stdin, fun () -> close_in stdin)
    | File path ->
       (try
          let chan = open_in path in
          let close () = close_in chan in
          Ok (Lexing.from_channel chan, close)
        with
          Sys_error msg -> Stdlib.Error (File_opening msg))
    | String s ->
        Ok (Lexing.from_string s, fun () -> ())
    | Channel chan ->
        let close () = close_in chan in
        Ok (Lexing.from_channel chan, close)
    | Buffer b -> Ok (b, fun () -> ()) in
  match buf_close_res with
    Ok (buffer, close) ->
      let () =
        match input with
          File path when path <> "" -> reset ~file:path buffer
        | _ -> () in
      let instance = {
        read; buffer; get_win; get_pos; get_last; get_file; close}
      in Ok instance
  | Error _ as e -> e

end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
