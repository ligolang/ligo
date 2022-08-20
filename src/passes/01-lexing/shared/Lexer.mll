(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* OCaml Stdlib *)

module Array = Caml.Array (* Used in the generated code only *)
module Int64 = Caml.Int64

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

(* The functorised interface *)

module type S = LexerLib.API.LEXER

module Make (Token : Token.S) =
  struct
    type token = Token.t
    module Core = LexerLib.Core

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Non_canonical_zero
    | Invalid_symbol of string
    | Wrong_nat_syntax of string
    | Wrong_mutez_syntax of string
    | Wrong_lang_syntax of string
    | Unterminated_verbatim of string
    | Invalid_linemarker_argument
    | Overflow_mutez
    | Underflow_mutez

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Invalid_symbol s ->
        sprintf "Invalid symbol: %S.\n\
                 Hint: Check the LIGO syntax you use." s
    | Wrong_nat_syntax hint ->
        sprintf "Wrong nat syntax.\n%s" hint
    | Wrong_mutez_syntax hint ->
        sprintf "Wrong mutez syntax.\n%s" hint
    | Wrong_lang_syntax hint ->
        sprintf "Wrong code injection syntax.\n%s" hint
    | Unterminated_verbatim term ->
        sprintf "Unterminated verbatim.\n\
                 Hint: Close with %S." term
    | Invalid_linemarker_argument ->
        "Unexpected or invalid linemarker argument.\n\
         Hint: The optional argument is either 1 or 2."
    | Overflow_mutez ->
        "Mutez amount too large.\n\
         Note: From 0 to 2^63-1=9_223_372_036_854_775_807."
    | Underflow_mutez ->
        "Mutez amount not an integer."

    type message = string Region.reg

    exception Error of message

    let fail region error =
      let msg = error_to_string error in
      raise (Error Region.{value=msg;region})

    let support_string_delimiter = Token.support_string_delimiter

    (* TOKENS *)

    (* Strings *)

    let mk_string (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_string lexeme region
      in Core.Token token, state

    (* Verbatim strings *)

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in Core.Token token, state

    (* Bytes *)

    let mk_bytes bytes state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let norm  = Str.(global_replace (regexp "_") "" bytes) in
      let token = Token.mk_bytes bytes norm region
      in Core.Token token, state

    (* Integers *)

    let mk_int state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let z = Z.of_string lexeme in
      if   Z.equal z Z.zero && String.(lexeme <> "0")
      then fail region Non_canonical_zero
      else let token = Token.mk_int lexeme z region
           in Core.Token token, state

    (* Natural numbers *)

    let mk_nat nat state buffer =
      let Core.{region; state; _} = state#sync buffer
      and z = Z.of_string nat in
      if   Z.equal z Z.zero && String.(nat <> "0")
      then fail region Non_canonical_zero
      else match Token.mk_nat nat z region with
             Ok token -> Core.Token token, state
           | Error Token.Wrong_nat_syntax hint ->
               fail region (Wrong_nat_syntax hint)

    (* Mutez *)

    let mk_mutez nat state buffer =
      let Core.{region; state; _} = state#sync buffer in
      match Int64.of_string_opt nat with
        None -> fail region Overflow_mutez
      | Some mutez_64 ->
          if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
          then fail region Non_canonical_zero
          else let suffix = "mutez" in
               match Token.mk_mutez nat ~suffix mutez_64 region with
                 Ok token -> Core.Token token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail region (Wrong_mutez_syntax hint)

    (* Integral Tez (internally converted to mutez) *)

    let mk_tez nat suffix state buffer =
      let Core.{region; state; _} = state#sync buffer
      and mutez = Z.mul (Z.of_int 1_000_000) (Z.of_string nat) in
      try
        let mutez_64 = Z.to_int64 mutez in
        if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
        then fail region Non_canonical_zero
        else match Token.mk_mutez nat ~suffix mutez_64 region with
               Ok token -> Core.Token token, state
             | Error Token.Wrong_mutez_syntax hint ->
                 fail region (Wrong_mutez_syntax hint)
      with Z.Overflow -> fail region Overflow_mutez

    (* Tez as a decimal number (internally converted to mutez) *)

    let mk_tez_dec integral fractional suffix state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let integral'   = Str.(global_replace (regexp "_") "" integral)
      and fractional' = Str.(global_replace (regexp "_") "" fractional) in
      let numerator   = Z.of_string (integral' ^ fractional')
      and frac_length = String.length fractional' in
      let denominator = Z.of_string ("1" ^ String.make frac_length '0')
      and million     = Q.of_string "1_000_000" in
      let q_mutez     = Q.make numerator denominator |> Q.mul million in
      if Z.equal (Q.den q_mutez) Z.one then
        try
          let mutez_64 = Z.to_int64 (Q.num q_mutez) in
          if   Int64.equal mutez_64 Int64.zero
               && String.(integral <> "0" || fractional <> "0")
          then fail region Non_canonical_zero
          else let lexeme = integral ^ "." ^ fractional in
               match Token.mk_mutez lexeme ~suffix mutez_64 region with
                 Ok token -> Core.Token token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail region (Wrong_mutez_syntax hint)
        with Z.Overflow -> fail region Overflow_mutez
      else fail region Underflow_mutez

    (* Identifiers *)

    let mk_ident state buffer =
      let Core.{region; lexeme; state} = state#sync buffer
      in Core.Token (Token.mk_ident lexeme region), state

    (* Attributes *)

    let mk_attr key value state buffer =
      let Core.{region; state; _} = state#sync buffer
      in Core.Token (Token.mk_attr ~key ?value region), state

    (* Data constructors and module names *)

    let mk_uident state buffer =
      let Core.{region; lexeme; state} = state#sync buffer
      in Core.Token (Token.mk_uident lexeme region), state

    (* Code injection *)

    let mk_lang start lang state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let start    = region#start#shift_bytes (String.length start) in
      let stop     = region#stop in
      let lang_reg = Region.make ~start ~stop in
      let lang     = Region.{value=lang; region=lang_reg} in
      match Token.mk_lang lang region with
        Ok token -> Core.Token token, state
      | Error Token.Wrong_lang_syntax hint ->
          fail region (Wrong_lang_syntax hint)

    (* Symbols *)

    let mk_sym state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token -> Core.Token token, state
      | Error Token.Invalid_symbol string ->
          fail region (Invalid_symbol string)

    (* End-of-File *)

    let mk_eof state buffer =
      let Core.{region; state; _} = state#sync buffer
      in Core.Token (Token.mk_eof region), state

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let nat        = natural as nat
let tz_or_tez  = "tz" | "tez" as tez
let decimal    = (natural as integral) '.' (natural as fractional)
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let letter     = small | capital
let ident      = small (letter | '_' | digit)*
               | '_' (letter | '_' (letter | digit) | digit)+
let ext_ident  = (letter | digit | '_' | ':')+
let uident     = capital (letter | '_' | digit)*
let string     = [^'"' '\\' '\n']*  (* For #include and attributes *)
let attr       = letter (letter | digit | '_' | ':' | '.' | '@' | '%')*
let hex_digit  = digit | ['A'-'F' 'a'-'f']
let byte       = hex_digit hex_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as bytes)
let directive  = '#' (blank* as space) (small+ as id) (* For #include *)
let code_inj   = ("[%" as start) (attr as lang)

(* Symbols *)

let     common_sym =   ";" | "," | "(" | ")"  | "[" | "]"  | "{" | "}"
                     | "=" | ":" | "|" | "." | "_" | "^"
                     | "+" | "-" | "*" | "/"  | "<" | "<=" | ">" | ">="
let  pascaligo_sym = "->" | "=/=" | "#" | ":="
let   cameligo_sym = "->" | "<>" | "::" | "||" | "&&" | "'" | "|>"
let reasonligo_sym = "!" | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"
let     jsligo_sym = "++" | "--" | "..." | "?" | "&" | "!" | "~" | "%"
                     | "<<<" | "==" | "!=" | "+=" | "-=" | "*=" | "/="
                     | "%=" | "<<<=" | "&=" | "|="
                     | "^=" | "=>" (* | ">>>" | ">>>=" *)

let symbol =
      common_sym
|  pascaligo_sym
|   cameligo_sym
| reasonligo_sym
|     jsligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is threaded
   through recursive calls. *)

rule scan state = parse
  ident
| '@' ext_ident { mk_ident            state lexbuf }
| uident        { mk_uident           state lexbuf }
| bytes         { mk_bytes bytes      state lexbuf }
| nat "n"       { mk_nat   nat        state lexbuf }
| nat "mutez"   { mk_mutez nat        state lexbuf }
| nat tz_or_tez { mk_tez   nat tez    state lexbuf }
| natural       { mk_int              state lexbuf }
| symbol        { mk_sym              state lexbuf }
| eof           { mk_eof              state lexbuf }
| code_inj      { mk_lang  start lang state lexbuf }

| "[@" (attr as key) (blank+ (string as value))? "]" {
    let value =
      match value with
        None -> None
      | Some string -> Some (Attr.String string)
    in mk_attr key value state lexbuf }

| decimal tz_or_tez {
    mk_tez_dec integral fractional tez state lexbuf }

| "`" | "{|" as lexeme {
    if String.equal lexeme @@ fst Token.verbatim_delimiters then
      let Core.{region; state; _} = state#sync lexbuf in
      let thread = Core.mk_thread region in
      let verb_end = snd Token.verbatim_delimiters
      in scan_verbatim verb_end thread state lexbuf |> mk_verbatim
    else
      let Core.{region; _} = state#sync lexbuf
      in fail region (Unexpected_character lexeme.[0]) }

| _ as c { let Core.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

(* Scanning verbatim strings *)

and scan_verbatim verbatim_end thread state = parse
  (* Inclusion of Michelson code *)
  '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* {
    let Core.{state; region; _} = state#sync lexbuf
    in eol verbatim_end region line file flag thread state lexbuf
  }
| nl as nl { let ()    = Lexing.new_line lexbuf
             and state = state#set_pos (state#pos#new_line nl) in
             scan_verbatim verbatim_end (thread#push_string nl) state lexbuf }
| eof      { fail thread#opening (Unterminated_verbatim verbatim_end) }
| "`"
| "|}" as lexeme  {
  if String.equal verbatim_end lexeme then
    Core.(thread, (state#sync lexbuf).state)
  else
    let Core.{state; _} = state#sync lexbuf in
    scan_verbatim verbatim_end (thread#push_string lexeme) state lexbuf }
| _ as c   { let Core.{state; _} = state#sync lexbuf in
             scan_verbatim verbatim_end (thread#push_char c) state lexbuf }

and eol verbatim_end region_prefix line file flag thread state = parse
  nl | eof { let _, state =
               Core.linemarker region_prefix ~line ~file ?flag state lexbuf
             in scan_verbatim verbatim_end thread state lexbuf }
| _        { let Core.{region; _} = state#sync lexbuf
             in fail region Invalid_linemarker_argument }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    (* Encoding a function call in exception-raising style (ERS) to
       error-passing style (EPS) *)

    let lift scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error msg -> Stdlib.Error msg

    (* Function [scan] is the main exported function *)

    let client : token Core.client =
      let open Simple_utils.Utils in
      object
        method mk_string = mk_string
        method mk_eof    = lift <@ mk_eof
        method callback  = lift <@ scan
        method support_string_delimiter = support_string_delimiter
      end

    let scan = Core.mk_scan client

  end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
