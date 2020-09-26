open Display
open Types

(*  this type is not necessary if the output of CLI command 'print-mini-c' have the
    same type when optimized and when not  *)
type optim =
  | Optimized of expression
  | Raw of program

let program_ppformat ~display_format (p,_) =
  match display_format with
  | Human_readable | Dev -> ( match p with
    | Optimized e -> (
      let buffer = Buffer.create 100 in
      let formatter = Format.formatter_of_buffer buffer in
      PP.expression formatter e;
      Format.pp_print_flush formatter ();
      (Location.dummy, Buffer.contents buffer)
    )
    | Raw e ->
      let buffer = Buffer.create 100 in
      let formatter = Format.formatter_of_buffer buffer in
      PP.program formatter e;
      Format.pp_print_flush formatter ();
      (Location.dummy, Buffer.contents buffer)
    )

let program_jsonformat (p,_) : json =
  let s = ( match p with
    | Optimized e -> Format.asprintf "%a" PP.expression e
    | Raw e -> Format.asprintf "%a" PP.program e ) in
  `Assoc [("Typed(temp)" , `String s)]

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}