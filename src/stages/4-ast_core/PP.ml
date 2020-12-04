[@@@coverage exclude_file]
(* open Stage_common.Types *)
open Types
open Format
open PP_helpers
include Stage_common.PP

  let record_sep value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let pp_atttr ppf s = match s with None -> fprintf ppf "" | Some s -> fprintf ppf "%s" s in
    let new_pp ppf (k, {associated_type;michelson_annotation;_}) = fprintf ppf "@[<h>%a -> %a [%a] @]" label k value associated_type pp_atttr michelson_annotation in
    fprintf ppf "%a" (list_sep new_pp sep) lst
  let variant_sep_d x = record_sep x (tag " ,@ ")

  let tuple_sep value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let record_sep_expr value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let tuple_sep_expr value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_,v) = fprintf ppf "%a" value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "

let rec type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te
and type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_variable        tv -> type_variable ppf tv
  | T_sum              m -> fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression) m.fields
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type type_expression) m.fields
  | T_arrow            a -> arrow         type_expression ppf a
  | T_app              a -> type_app type_expression ppf a
  | T_module_accessor ma -> module_access type_expression ppf ma


let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal          l -> literal                    ppf l
  | E_variable         n -> expression_variable        ppf n
  | E_application      a -> application     expression ppf a
  | E_constructor      c -> constructor     expression ppf c
  | E_constant         c -> constant        expression ppf c
  | E_record           r -> record          expression ppf r
  | E_record_accessor ra -> record_accessor expression ppf ra
  | E_record_update   ru -> record_update   expression ppf ru
  | E_lambda    l -> lambda expression type_expression ppf l
  | E_recursive r -> recursive expression type_expression ppf r
  | E_matching {matchee; cases; _} ->
      fprintf ppf "@[match %a with@ %a@]" expression matchee (matching expression)
        cases
  | E_let_in { let_binder ;rhs ; let_result; inline } ->
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]" (binder type_expression) let_binder expression rhs option_inline inline expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_raw_code r -> raw_code expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression ppf ma

and matching_variant_case : (_ -> expression -> unit) -> _ -> match_variant -> unit =
  fun f ppf {constructor=c ; proj ; body } ->
  fprintf ppf "| %a %a ->@;<1 2>%a@ " label c expression_variable proj f body

and matching : (formatter -> expression -> unit) -> formatter -> matching_expr -> unit =
  fun f ppf m -> match m with
    | Match_variant lst ->
        fprintf ppf "@[<hv>%a@]" (list_sep (matching_variant_case f) (tag "@ ")) lst
    | Match_list {match_nil ; match_cons = {hd; tl; body}} ->
        fprintf ppf "@[<hv>| Nil ->@;<1 2>%a@ | %a :: %a ->@;<1 2>%a@]"
          f match_nil expression_variable hd expression_variable tl f body
    | Match_option {match_none ; match_some = {opt; body}} ->
        fprintf ppf "@[<hv>| None ->@;<1 2>%a@ | Some %a ->@;<1 2>%a@]" f match_none expression_variable opt f body

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"

and matching_variant_case_type ppf {constructor=c ; proj ; body=_ } =
  fprintf ppf "| %a %a" label c expression_variable proj

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type     dt -> declaration_type                type_expression ppf dt
  | Declaration_constant {binder=b ; attr ; expr} ->
      fprintf ppf "@[<2>const %a =@ %a%a@]"
        (binder type_expression) b
        expression expr
        option_inline attr.inline


let program ppf (p : program) = program declaration ppf p
