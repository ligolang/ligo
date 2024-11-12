(* Lexing numbers *)

{
(* START HEADER *)

open Core
open Ast

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Wrap   = Lexing_shared.Wrap

(* Third-party libraries *)

module Array  = Stdlib.Array  (* Used in the generated code only *)

(* UTILITIES *)

let ( let* ) v f = Result.bind v ~f

let make_bytes comments lexbuf literal region : (string * Hex.t) wrap =
  let normalised = Str.(global_replace (regexp "_") "" literal)
  and lexeme = Lexing.lexeme lexbuf in
  Wrap.make ~comments (lexeme, `Hex normalised) region

let make_hex comments lexbuf literal region is_big : number =
  Hex (make_bytes comments lexbuf literal region, is_big)

let make_bin comments lexbuf literal region is_big : number =
  Bin (make_bytes comments lexbuf literal region, is_big)

let make_oct comments lexbuf literal region is_big : number =
  Oct (make_bytes comments lexbuf literal region, is_big)

let make_dec comments lexbuf ?(integral="0") ?(fractional="0") ?(exponent="")
             region is_big : number =
  let lexeme = Lexing.lexeme lexbuf
  and q = Q.of_string (integral ^ "." ^ fractional ^ exponent)
  in Dec (Wrap.make ~comments (lexeme, q) region, is_big)

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let digit = ['0'-'9']
let decimalDigits = digit ('_'? digit)*
let signedInteger = ('-' | '+')? decimalDigits
let exponentPart = ('e' | 'E')? signedInteger as exponent

let decimalIntegerLiteral = ('0' | '0'? ['1'-'9'] ('_'? decimalDigits)?)

let integralPart = decimalIntegerLiteral as integral
let fractionalPart = decimalDigits as fractional
let natural = decimalDigits as nat

(*
let decimalLiteral =
  integralPart '.' fractionalPart? exponentPart?
| '.' fractionalPart exponentPart?
| integralPart exponentPart
| natural *)

let byte = digit | ['A'-'F' 'a'-'f']
let hexLiteral = ("0x" | "0X") (byte | byte ('_'? byte)* as hex)

let bit = ['0'-'1']
let binaryLiteral = ("0b" | "0B") (bit | bit ('_'? bit)* as bin)

let octal = ['0'-'8']
let octalLiteral = ("0o" | "0O") (octal | octal ('_'? octal)* as oct)

(*
let bigintLiteral =
  hexLiteral 'n' | binaryLiteral 'n' | octalLiteral 'n' | natural 'n' *)

(* RULES (SCANNERS) *)

rule scan comments region = parse
  hexLiteral     { make_hex comments lexbuf hex region false }
| binaryLiteral  { make_bin comments lexbuf bin region false }
| octalLiteral   { make_oct comments lexbuf oct region false }
(* bigintLiteral *)
| hexLiteral 'n'    { make_hex comments lexbuf hex region true }
| binaryLiteral 'n' { make_bin comments lexbuf bin region true }
| octalLiteral 'n'  { make_oct comments lexbuf oct region true }
| natural 'n'       { make_dec comments lexbuf ~integral:nat region true }
(* decimalLiteral *)
| integralPart '.' fractionalPart? exponentPart? {
    make_dec comments lexbuf ~integral ?fractional ?exponent region false
  }
| '.' fractionalPart exponentPart? {
    make_dec comments lexbuf ~fractional ?exponent region false
  }
| integralPart exponentPart {
    make_dec comments lexbuf ~integral ~exponent region false
  }
| natural { make_dec comments lexbuf ~integral:nat region false }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* END TRAILER *)
}
