open Format
open Simple_utils.PP_helpers


(* Types *)
let type_record type_expression ppf = fun record ->
  Format.fprintf ppf "%a" (Rows.PP.tuple_or_record_sep_type type_expression) record

let type_tuple type_expression ppf = fun tuple ->
  fprintf ppf "(%a)" (list_sep type_expression (tag " , ")) tuple

let wildcard ppf = fun () ->
  fprintf ppf "_"

(* Expressions *)


and single_record_patch expression ppf ((p, expr) : Label.t * 'expr) =
  Format.fprintf ppf "%a <- %a" Label.pp p expression expr
let lst expression ppf = fun lst ->
  fprintf ppf "list[%a]" (list_sep_d expression) lst

let set expression ppf = fun set ->
  fprintf ppf "set[%a]" (list_sep_d expression) set

(* matches *)
