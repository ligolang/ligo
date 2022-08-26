[@@@coverage exclude_file]
module Location    = Simple_utils.Location
module Var         = Simple_utils.Var
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Types
open Format
open Simple_utils.PP_helpers
include Stage_common.PP

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_t value sep ppf m =
  assert (Stage_common.Helpers.is_tuple_lmap m);
  let lst = Stage_common.Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Stage_common.Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_t value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_t value (tag sep_record)) m

let lmap_sep_d x = lmap_sep x (tag " ,@ ")
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

open Format

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> type_variable                 ppf tv
  | T_constant        tc -> type_injection ppf tc
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row_element) (LMap.to_kv_list_rev m.content)
  | T_record           m -> fprintf ppf "%a" record m
  | T_arrow            a -> arrow         type_expression ppf a
  | T_singleton       x  -> literal       ppf             x
  | T_for_all         x  -> for_all       type_expression ppf x

and row_element : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a" type_expression associated_type
    (*
    fprintf ppf "{associated_type %a ; pos %i}"
      type_expression associated_type
      decl_pos
*)
and type_injection ppf {language;injection;parameters} =
  ignore language;
  fprintf ppf "[%s {| %s %a |}]" language (Stage_common.Constant.to_string injection) (list_sep_d_par type_expression) parameters

and record ppf {content; layout=_} =
  fprintf ppf "%a"
    (tuple_or_record_sep_type type_expression) content

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te.type_content

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" ValueVar.pp ev


let rec expression ppf ({ expression_content=ec; location=_; type_expression=te } : expression) =
  fprintf ppf "(%a : %a)"
    expression_content ec
    type_expression te

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant' c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_lambda {binder=b; result} ->
      fprintf ppf "lambda (%a) return %a" (binder type_expression) b
        expression result
  | E_type_abstraction e -> type_abs expression ppf e
  | E_matching {matchee; cases;} ->
      fprintf ppf "@[<v 2> match @[%a@] with@ %a@]" expression matchee (matching expression) cases
  | E_let_in {let_binder; rhs; let_result; attr = { inline; no_mutation; public=_ ; view = _ ; hidden = _ ; thunk } } ->
      fprintf ppf "@[let %a = %a%a%a%a in@.%a@]" (binder type_expression) let_binder expression
        rhs option_inline inline option_no_mutation no_mutation option_thunk thunk expression let_result
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_recursive { fun_name;fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_assign a -> assign expression type_expression ppf a
  | E_type_inst {forall;type_} ->
      fprintf ppf "%a@@{%a}" expression forall type_expression type_

and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (_ -> expression -> unit) -> _ -> matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "@[<v 2>| %a %a ->@ %a@]" label c expression_variable pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "@[%a@]" (list_sep (matching_variant_case f) (tag "@ ")) cases
  | Match_record {fields ; body ; tv = _} ->
      let fields = LMap.map (fun b -> b.var) fields in
      fprintf ppf "| @[%a@] ->@ @[%a@]"
        (tuple_or_record_sep_expr expression_variable) fields
        f body


let program ppf p = expression ppf p
