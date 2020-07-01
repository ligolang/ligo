[@@@coverage exclude_file]
open Types
open Format
open PP_helpers
module Helpers = Stage_common.Helpers
include Stage_common.PP


  let cmap_sep value sep ppf m =
    let lst = CMap.to_kv_list m in
    let lst = List.sort (fun (Constructor a,_) (Constructor b,_) -> String.compare a b) lst in
    let new_pp ppf (k, {ctor_type;_}) = fprintf ppf "@[<h>%a -> %a@]" constructor k value ctor_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst
  let cmap_sep_d x = cmap_sep x (tag " ,@ ")

  let record_sep value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, {field_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value field_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let tuple_sep value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_, {field_type;_}) = fprintf ppf "%a" value field_type in
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

(* Prints records which only contain the consecutive fields
  0..(cardinal-1) as tuples *)
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_expr value (tag sep_record)) m

let tuple_or_record_sep_expr value = tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.content with
  | T_sum m -> fprintf ppf "@[<hv 4>sum[%a]@]" (cmap_sep_d type_expression) m
  | T_record m -> fprintf ppf "%a" (tuple_or_record_sep_type type_expression) m
  | T_arrow a -> fprintf ppf "%a -> %a" type_expression a.type1 type_expression a.type2
  | T_variable tv -> type_variable ppf tv
  | T_constant tc -> type_constant ppf tc
  | T_operator to_ -> type_operator type_expression ppf to_

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

and type_operator : (formatter -> type_expression -> unit) -> formatter -> type_operator * type_expression list -> unit =
  fun f ppf to_ ->
  let s = match to_ with
    TC_option                    , lst -> Format.asprintf "option(%a)"                     (list_sep_d f) lst
  | TC_list                      , lst -> Format.asprintf "list(%a)"                       (list_sep_d f) lst
  | TC_set                       , lst -> Format.asprintf "set(%a)"                        (list_sep_d f) lst
  | TC_map                       , lst -> Format.asprintf "Map (%a)"                       (list_sep_d f) lst
  | TC_big_map                   , lst -> Format.asprintf "Big Map (%a)"                   (list_sep_d f) lst
  | TC_map_or_big_map            , lst -> Format.asprintf "Map Or Big Map (%a)"            (list_sep_d f) lst
  | TC_contract                  , lst -> Format.asprintf "Contract (%a)"                  (list_sep_d f) lst
  | TC_michelson_pair            , lst -> Format.asprintf "michelson_pair (%a)"            (list_sep_d f) lst                            
  | TC_michelson_or              , lst -> Format.asprintf "michelson_or (%a)"              (list_sep_d f) lst
  | TC_michelson_pair_right_comb , lst -> Format.asprintf "michelson_pair_right_comb (%a)" (list_sep_d f) lst
  | TC_michelson_pair_left_comb  , lst -> Format.asprintf "michelson_pair_left_comb (%a)"  (list_sep_d f) lst
  | TC_michelson_or_right_comb   , lst -> Format.asprintf "michelson_or_right_comb (%a)"   (list_sep_d f) lst
  | TC_michelson_or_left_comb    , lst -> Format.asprintf "michelson_or_left_comb (%a)"    (list_sep_d f) lst
  in
  fprintf ppf "(type_operator: %s)" s

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  expression_content ppf e.content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "@[<hv>(%a)@@(%a)@]" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "@[%a(%a)@]" constructor c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "@[%a.%a@]" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "@[{ %a@;<1 2>with@;<1 2>{ %a = %a } }@]" expression record label path expression update
  | E_lambda {binder; input_type; output_type; result} ->
      fprintf ppf "@[lambda (%a:%a) : %a@ return@ %a@]"
        expression_variable binder
        (PP_helpers.option type_expression)
        input_type
        (PP_helpers.option type_expression)
        output_type expression result
  | E_recursive { fun_name; fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )" 
        expression_variable fun_name 
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_matching {matchee; cases; _} ->
      fprintf ppf "@[match %a with@ %a@]" expression matchee (matching expression)
        cases
  | E_let_in { let_binder ;rhs ; let_result; inline } ->    
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]" option_type_name let_binder expression rhs option_inline inline expression let_result
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "%a : %a" expression anno_expr type_expression
        type_annotation

and option_type_name ppf
    ((n, ty_opt) : expression_variable * type_expression option) =
  match ty_opt with
  | None ->
      fprintf ppf "%a" expression_variable n
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable n type_expression ty

and assoc_expression ppf : expr * expr -> unit =
 fun (a, b) -> fprintf ppf "@[<2>%a ->@;<1 2>%a@]" expression a expression b

and single_record_patch ppf ((p, expr) : label * expr) =
  fprintf ppf "%a <- %a" label p expression expr

and matching_variant_case : (_ -> expression -> unit) -> _ -> (constructor' * expression_variable) * expression -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a ->@;<1 2>%a@ " constructor c expression_variable n f a

and matching : (formatter -> expression -> unit) -> formatter -> matching_expr -> unit =
  fun f ppf m -> match m with
    | Match_variant lst ->
        fprintf ppf "@[<hv>%a@]" (list_sep (matching_variant_case f) (tag "@ ")) lst
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "@[<hv>| Nil ->@;<1 2>%a@ | %a :: %a ->@;<1 2>%a@]" f match_nil expression_variable hd expression_variable tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "@[<hv>| None ->@;<1 2>%a@ | Some %a ->@;<1 2>%a@]" f match_none expression_variable some f match_some

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"

and matching_variant_case_type ppf ((c,n),_a) =
  fprintf ppf "| %a %a" constructor c expression_variable n

and option_mut ppf mut = 
  if mut then 
    fprintf ppf "[@@mut]"
  else
    fprintf ppf ""

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type (type_name, te) ->
      fprintf ppf "@[<2>type %a =@ %a@]" type_variable type_name type_expression te
  | Declaration_constant (name, ty_opt, attr, expr) ->
      fprintf ppf "@[<2>const %a =@ %a%a@]" option_type_name (name, ty_opt) expression
        expr
        option_inline attr.inline

let program ppf (p : program) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
