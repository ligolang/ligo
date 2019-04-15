module Region = Tezos_utils.Region

type lexeme = string

type t =
  Tabs      of int    Region.reg
| Space     of int    Region.reg
| Newline   of lexeme Region.reg
| LineCom   of lexeme Region.reg
| BlockCom  of lexeme Region.reg
| BOM       of lexeme Region.reg

type markup = t

(* Pretty-printing *)

let sprintf = Printf.sprintf

let to_lexeme = function
  Tabs     Region.{value;_} -> String.make value '\t'
| Space    Region.{value;_} -> String.make value ' '
| Newline  Region.{value;_}
| LineCom  Region.{value;_}
| BlockCom Region.{value;_}
| BOM      Region.{value;_} -> value

let to_string markup ?(offsets=true) mode =
  let region, val_str =
    match markup with
      Tabs Region.{value; region} ->
        let lex = String.make value '\t' |> String.escaped
        in region, sprintf "Tabs \"%s\"" lex
    | Space Region.{value; region} ->
        region, sprintf "Space \"%s\"" (String.make value ' ')
    | Newline Region.{value; region} ->
        region, sprintf "Newline \"%s\"" (String.escaped value)
    | LineCom Region.{value; region} ->
        region, sprintf "LineCom \"%s\"" (String.escaped value)
    | BlockCom Region.{value; region} ->
        region, sprintf "BlockCom \"%s\"" (String.escaped value)
    | BOM Region.{value; region} ->
        region, sprintf "BOM \"%s\"" (String.escaped value) in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str
