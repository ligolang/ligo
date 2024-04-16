(* This module defines the sorts of markup recognised by the lexer *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Markup *)

type lexeme = string

type markup =
  Tabs      of int    Region.reg
| Space     of int    Region.reg
| Newline   of lexeme Region.reg
| LineCom   of lexeme Region.reg
| BlockCom  of lexeme Region.reg
| BOM       of lexeme Region.reg

type t = markup

(* Projecting regions *)

let to_region = function
  Tabs     Region.{region; _}
| Space    Region.{region; _}
| Newline  Region.{region; _}
| LineCom  Region.{region; _}
| BlockCom Region.{region; _}
| BOM      Region.{region; _} -> region


(* Pretty-printing *)

let sprintf = Printf.sprintf

let to_lexeme = function
  Tabs     Region.{value;_} -> String.make value '\t'
| Space    Region.{value;_} -> String.make value ' '
| Newline  Region.{value;_}
| LineCom  Region.{value;_}
| BlockCom Region.{value;_}
| BOM      Region.{value;_} -> value

(* Note: %S implies escaping strings à la OCaml *)

let to_string ~offsets mode markup =
  let region, val_str =
    match markup with
      Tabs Region.{value; region} ->
        let lex = String.make value '\t'
        in region, sprintf "Tabs %S" lex
    | Space Region.{value; region} ->
        region, sprintf "Space %S" (String.make value ' ')
    | Newline Region.{value; region} ->
       region, sprintf "Newline %S" value
    | LineCom Region.{value; region} ->
        region, sprintf "LineCom %S" value
    | BlockCom Region.{value; region} ->
        region, sprintf "BlockCom %S" value
    | BOM Region.{value; region} ->
        region, sprintf "BOM %S" value in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

(* Comments *)

type basic_comment =
  Line  of lexeme Region.reg
| Block of lexeme Region.reg
