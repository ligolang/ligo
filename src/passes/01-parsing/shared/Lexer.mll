(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

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
    | Reserved_name of string
    | Invalid_symbol
    | Invalid_natural
    (*    | Invalid_attribute*)
    | Unterminated_verbatim

      (* Style errors *)
    | Odd_lengthed_bytes
    | Missing_break
    | Negative_byte_sequence

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Reserved_name s ->
        sprintf "Reserved name: \"%s\".\n\
         Hint: Change the name." s
    | Invalid_symbol ->
        "Invalid symbol.\n\
         Hint: Check the LIGO syntax you use."
    | Invalid_natural ->
        "Invalid natural number."
    | Unterminated_verbatim ->
       "Unterminated verbatim.\n\
        Hint: Close with \"|}\"."
    | Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit."
    | Missing_break ->
       "Missing break.\n\
        Hint: Insert some space."
    | Negative_byte_sequence ->
       "Negative byte sequence.\n\
        Hint: Remove the leading minus sign."

    type message = string Region.reg

    exception Error of message

    let fail region error =
      let msg = error_to_string error in
      raise (Error Region.{value=msg;region})

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
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in state#enqueue token

    let mk_int state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural

    let mk_mutez state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_mutez lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_tez state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
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
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
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
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_ident lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Reserved_name ->
          fail region (Reserved_name lexeme)

    let mk_attr attr state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_attr attr region
      in state#enqueue token

    let mk_constr state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      let token = Token.mk_constr lexeme region
      in state#enqueue token

    let mk_lang lang state buffer =
      let Core.{region; state; _}
                             = state#sync buffer in
      let start              = region#start#shift_bytes 1 in
      let stop               = region#stop in
      let lang_reg           = Region.make ~start ~stop in
      let lang               = Region.{value=lang; region=lang_reg} in
      let token              = Token.mk_lang lang region
      in state#enqueue token

    let mk_sym state buffer =
      let open Core in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Invalid_symbol -> fail region Invalid_symbol

    let mk_eof state buffer =
      let open Core in
      let {region; state; _} = state#sync buffer in
      let token = Token.eof region
      in state#enqueue token

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

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
let attr       = letter (letter | '_' | ':' | digit)*
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as seq)
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)

(* Symbols *)

let common_sym     =   ';' | ',' | '(' | ')'  | '[' | ']'  | '{' | '}'
                     | '=' | ':' | '|' | '.' | '_'  | '^'
                     | '+' | '-' | '*' | '/'  | '<' | "<=" | '>' | ">="
let pascaligo_sym  = "->" | "=/=" | '#' | ":="
let cameligo_sym   = "->" | "<>" | "::" | "||" | "&&"
let reasonligo_sym = '!' | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"

let symbol = common_sym | pascaligo_sym | cameligo_sym | reasonligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is thread through
   recursive calls. *)

rule scan state = parse
  ident                  { mk_ident        state lexbuf }
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
| "[@"  (attr as a) "]"  { mk_attr       a state lexbuf }
| "[%"  (attr as l)      { mk_lang       l state lexbuf }

| "{|" {
    let Core.{region; state; _} = state#sync lexbuf in
    let thread = Core.mk_thread region
    in scan_verbatim thread state lexbuf |> mk_verbatim }

| _ as c { let Core.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

and scan_verbatim thread state = parse
  nl as nl { let ()    = Lexing.new_line lexbuf
             and state = state#set_pos (state#pos#new_line nl) in
             scan_verbatim (thread#push_string nl) state lexbuf }
| '#' blank* (natural as line) blank+ '"' (string as file) '"' {
             let state = Core.line_preproc ~line ~file state lexbuf
             in scan_verbatim thread state lexbuf }
| eof      { fail thread#opening Unterminated_verbatim }
| "|}"     { Core.(thread, (state#sync lexbuf).state) }
| _ as c   { let Core.{state; _} = state#sync lexbuf in
             scan_verbatim (thread#push_char c) state lexbuf }

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
    end

let scan = Core.mk_scan client

(* Style checking *)

let check_right_context config token next_token lexbuf =
  let pos    = (config#to_region token)#stop in
  let region = Region.make ~start:pos ~stop:pos in
  let next lexbuf =
    match next_token lexbuf with
      None -> ()
    | Some (markup, next) ->
        let open Token in
        if   is_minus token && is_bytes next
        then let region =
               Region.cover (to_region token) (to_region next)
             in fail region Negative_byte_sequence
        else
          match markup with
            [] ->
              if   is_int token || is_string token
              then if   is_sym next || is_eof next
                   then ()
                   else fail region Missing_break
              else
                if   is_bytes token
                then if   is_int next || is_hexa next
                     then fail region Odd_lengthed_bytes
                     else
                       if   is_sym next || is_eof next
                       then ()
                       else fail region Missing_break
                else ()
          | _::_ -> ()
  in lift next lexbuf

end (* of functor [Make] in HEADER *)

(* END TRAILER *)
}
