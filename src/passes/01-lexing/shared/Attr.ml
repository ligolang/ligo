(* Attributes *)

type key = string

type value =
  String of string
| Ident  of string

type attribute = key * value option

type t = attribute

let sprintf = Printf.sprintf

let to_lexeme (key, value_opt) =
  match value_opt with
    None -> sprintf "[@%s]" key
  | Some String value -> sprintf "[@%s %S]" key value
  | Some Ident value ->  sprintf "[@%s %s]" key value

let to_string (key, value_opt) =
  match value_opt with
    None -> sprintf "%S" key
  | Some String value -> sprintf "(%S, Some (String %S))" key value
  | Some Ident value -> sprintf "(%S, Some (Ident %S))" key value
