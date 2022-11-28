open Simple_utils.Display
open Types

(*  this type is not necessary if the output of CLI command 'print-mini-c' have the
    same type when optimized and when not  *)
type optim =
  | Optimized of expression
  | Raw of expression

let program_ppformat ~display_format f p =
  match display_format with
  | Human_readable | Dev ->
    (match p with
    | Optimized e -> PP.expression f e
    | Raw e -> PP.expression f e)


let program_jsonformat p : json =
  let s =
    match p with
    | Optimized e -> Format.asprintf "%a" PP.expression e
    | Raw e -> Format.asprintf "%a" PP.expression e
  in
  `Assoc [ "Typed(temp)", `String s ]


let program_format : 'a format = { pp = program_ppformat; to_json = program_jsonformat }
