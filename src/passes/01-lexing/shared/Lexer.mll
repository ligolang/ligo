(* Lexer specification for all LIGO dialects, to be processed by
   [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* OCaml Stdlib *)

module Array = Caml.Array (* Used in the generated code only *)
module Int64 = Caml.Int64

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

module Options   = LexerLib.Options   (* For instantiation only *)
module Unit      = LexerLib.Unit      (* For instantiation only *)
module Client    = LexerLib.Client    (* For the interface only *)
module Directive = Preprocessor.Directive (* For verbatim only  *)
module State     = LexerLib.State
module Thread    = LexerLib.Thread

(* The functorised interface *)

module Make (Options : Options.S) (Token : Token.S) =
  struct
    type token = Token.t

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Non_canonical_zero
    | Invalid_symbol of string
    | Wrong_nat_syntax of string
    | Wrong_mutez_syntax of string
    | Wrong_lang_syntax of string
    | Unterminated_verbatim of string
    | Overflow_mutez
    | Underflow_mutez
    | Invalid_directive of Preprocessor.Error.t

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
    | Overflow_mutez ->
        "Mutez amount too large.\n\
         Note: From 0 to 2^63-1=9_223_372_036_854_775_807."
    | Underflow_mutez ->
        "Mutez amount not an integer."
    | Invalid_directive err ->
        Preprocessor.Error.to_string err

    (* Raising the exception for lexical errors *)

    type message = string Region.reg

    exception Error of message

    let fail (region: Region.t) error =
      let msg = error_to_string error in
      raise (Error Region.{value=msg; region})

    (* TOKENS *)

    (* Strings *)

    let mk_string thread =
      let start  = thread#opening#start in
      let stop   = thread#closing#stop in
      let region = Region.make ~start ~stop in
      Token.mk_string (thread#to_string) region

    (* Verbatim strings *)

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in token, state

    (* Bytes *)

    let mk_bytes bytes state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let norm  = Str.(global_replace (regexp "_") "" bytes) in
      let token = Token.mk_bytes bytes norm region
      in token, state

    (* Integers *)

    let mk_int state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let lexeme = value in
      let z = Z.of_string lexeme in
      if   Z.equal z Z.zero && String.(lexeme <> "0")
      then fail region Non_canonical_zero
      else let token = Token.mk_int lexeme z region
           in token, state

    (* Natural numbers *)

    let mk_nat nat state buffer =
      let state, Region.{region; _} = state#sync buffer
      and z = Z.of_string nat in
      if   Z.equal z Z.zero && String.(nat <> "0")
      then fail region Non_canonical_zero
      else match Token.mk_nat nat z region with
             Ok token -> token, state
           | Error Token.Wrong_nat_syntax hint ->
               fail region (Wrong_nat_syntax hint)

    (* Mutez *)

    let mk_mutez nat state buffer =
      let state, Region.{region; _} = state#sync buffer in
      match Int64.of_string_opt nat with
        None -> fail region Overflow_mutez
      | Some mutez_64 ->
          if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
          then fail region Non_canonical_zero
          else let suffix = "mutez" in
               match Token.mk_mutez nat ~suffix mutez_64 region with
                 Ok token -> token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail region (Wrong_mutez_syntax hint)

    (* Integral Tez (internally converted to mutez) *)

    let mk_tez nat suffix state buffer =
      let state, Region.{region; _} = state#sync buffer
      and mutez = Z.mul (Z.of_int 1_000_000) (Z.of_string nat) in
      try
        let mutez_64 = Z.to_int64 mutez in
        if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
        then fail region Non_canonical_zero
        else match Token.mk_mutez nat ~suffix mutez_64 region with
               Ok token -> token, state
             | Error Token.Wrong_mutez_syntax hint ->
                 fail region (Wrong_mutez_syntax hint)
      with Z.Overflow -> fail region Overflow_mutez

    (* Tez as a decimal number (internally converted to mutez) *)

    let mk_tez_dec integral fractional suffix state buffer =
      let state, Region.{region; _} = state#sync buffer in
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
                 Ok token -> token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail region (Wrong_mutez_syntax hint)
        with Z.Overflow -> fail region Overflow_mutez
      else fail region Underflow_mutez

    (* Identifiers *)

    let mk_ident state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let token = Token.mk_ident value region
      in token, state

    (* Attributes *)

    let mk_attr key value state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let token = Token.mk_attr ~key ?value region
      in token, state

    (* Data constructors and module names *)

    let mk_uident state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let token = Token.mk_uident value region
      in token, state

    (* Code injection *)

     let mk_lang start lang state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let start    = region#start#shift_bytes (String.length start) in
      let stop     = region#stop in
      let lang_reg = Region.make ~start ~stop in
      let lang     = Region.{value=lang; region=lang_reg} in
      match Token.mk_lang lang region with
        Ok token -> token, state
      | Error Token.Wrong_lang_syntax hint ->
          fail region (Wrong_lang_syntax hint)

    (* Symbols *)

    let mk_sym state buffer =
      let state, Region.{region; value} = state#sync buffer in
      match Token.mk_sym value region with
        Ok token -> token, state
      | Error Token.Invalid_symbol string ->
          fail region (Invalid_symbol string)

    (* End-of-File *)

    let mk_eof state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let token = Token.mk_eof region
      in token, state

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
let ext_ident  = (letter | digit | '_')+
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
                     | "=" | ":" | "|" | "." | "_"
                     | "+" | "-" | "*" | "/"  | "<" | "<=" | ">" (*| ">="*)
let  pascaligo_sym = "->" | "=/=" | "#" | ":=" | "^"
let   cameligo_sym = "->" | "<>" | "::" | "||" | "&&" | "'" | "|>" | "^"
let reasonligo_sym = "!" | "=>" | "!=" | "==" | "++" | "..."
                     | "||" | "&&" | "^"
let     jsligo_sym =   "..." | "?" | "!" | "%" | "==" | "!=" | "+=" | "-="
                     | "*=" | "/="| "%=" | "=>"
let     pyligo_sym = "->" | "^"   | "**"  | "//" | "%"  | "@"  | "|" | "&"
                   | "~"  | "`"   | "\\"
                   | "==" | "!=" | "+=" | "-="
                   | "*=" | "/="  | "//=" | "%=" | "@=" | "&=" | "|="
                   | "^=" | "<<=" | "**=" (* | ">>=" *)
let symbol =
      common_sym
|  pascaligo_sym
|   cameligo_sym
| reasonligo_sym
|     jsligo_sym
|     pyligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is threaded through
   recursive calls. We start with the special cases so if they fail in
   their semantic actions, the normal cases can be tried next. *)

rule scan state = parse
  "[@" (attr as key) (blank+ (string as value))? "]" {
    let value =
      match value with
        None        -> None
      | Some string -> Some (Attr.String string)
    in mk_attr key value state lexbuf }

| "`" | "{|" as lexeme {
    let verb_open, verb_close = Token.verbatim_delimiters in
    if String.(lexeme = verb_open) then
      let state, Region.{region; _} = state#sync lexbuf in
      let thread = Thread.make ~opening:region
      in scan_verbatim verb_close thread state lexbuf
         |> mk_verbatim
    else mk_sym state lexbuf }

| ident
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
| decimal tz_or_tez {
    mk_tez_dec integral fractional tez state lexbuf }

| _ as c { let _, Region.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

(* Scanning verbatim strings with or without inclusion of Michelson
   code *)

and scan_verbatim verb_close thread state = parse
  (* Here is a hack to scan and ignore linemarkers in verbatim
     strings. Those could be the result of preprocessing an #include
     directive, or manually inserted. *)

  '#' blank* (natural as linenum) {
    let hash_state    = state in
    let state, _      = state#sync lexbuf in
    let preproc_state = new Preprocessor.State.t state#pos in
    let linenum       = Region.wrap_ghost linenum in (* We don't care. *)
    (match Directive.scan_linemarker
             hash_state#pos linenum preproc_state lexbuf
     with Stdlib.Error (region, error) ->
            fail region (Invalid_directive error)
        | Ok _ ->
            let state = hash_state#newline lexbuf in
            scan_verbatim verb_close thread state lexbuf) }

| "`" | "|}" as lexeme {
    if String.(verb_close = lexeme) then
      thread, fst (state#sync lexbuf)
    else
      let state, _ = state#sync lexbuf
      and thread   = thread#push_string lexeme in
      scan_verbatim verb_close thread state lexbuf }

| nl  { let nl     = Lexing.lexeme lexbuf in
        let ()     = Lexing.new_line lexbuf
        and state  = state#set_pos (state#pos#new_line nl)
        and thread = thread#push_string nl in
        scan_verbatim verb_close thread state lexbuf }

| eof { fail thread#opening (Unterminated_verbatim verb_close) }

| _   { let lexeme   = Lexing.lexeme lexbuf in
        let state, _ = state#sync lexbuf
        and thread   = thread#push_string lexeme in
        scan_verbatim verb_close thread state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    let callback state lexbuf =
      try Stdlib.Ok (scan state lexbuf) with
        Error msg -> Stdlib.Error msg

    let mk_eof = Token.mk_eof (* For EOFs from the preprocessor *)

  end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
