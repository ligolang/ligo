[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP
include Ast_PP_type(Ast_simplified_parameter)

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev


let rec expression ppf (e : expression) =
  match e.expression_content with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application app ->
      fprintf ppf "(%a)@(%a)" expression app.expr1 expression app.expr2
  | E_constructor c ->
      fprintf ppf "%a(%a)" constructor c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.expr label ra.label
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_map m ->
      fprintf ppf "map[%a]" (list_sep_d assoc_expression) m
  | E_big_map m ->
      fprintf ppf "big_map[%a]" (list_sep_d assoc_expression) m
  | E_list lst ->
      fprintf ppf "list[%a]" (list_sep_d expression) lst
  | E_set lst ->
      fprintf ppf "set[%a]" (list_sep_d expression) lst
  | E_look_up (ds, ind) ->
      fprintf ppf "(%a)[%a]" expression ds expression ind
  | E_lambda {binder; input_type; output_type; result} ->
      fprintf ppf "lambda (%a:%a) : %a return %a" option_type_name binder
        (PP_helpers.option type_expression)
        input_type
        (PP_helpers.option type_expression)
        output_type expression result
  | E_matching {matchee; cases; _} ->
      fprintf ppf "match %a with %a" expression matchee (matching expression)
        cases
  | E_let_in { let_binder ; mut; rhs ; let_result; inline } ->    
    fprintf ppf "let %a%a = %a%a in %a" option_mut mut option_type_name let_binder expression rhs option_inline inline expression let_result
  | E_skip ->
      fprintf ppf "skip"
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
 fun (a, b) -> fprintf ppf "%a -> %a" expression a expression b

and single_record_patch ppf ((p, expr) : label * expr) =
  fprintf ppf "%a <- %a" label p expression expr

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (constructor' * expression_variable) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a -> %a" constructor c expression_variable n f a

and matching : type a . (formatter -> a -> unit) -> formatter -> (a,unit) matching_content -> unit =
  fun f ppf m -> match m with
    | Match_tuple ((lst, b), _) ->
        fprintf ppf "let (%a) = %a" (list_sep_d expression_variable) lst f b
    | Match_variant (lst, _) ->
        fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons, _)} ->
        fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil expression_variable hd expression_variable tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some, _)} ->
        fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none expression_variable some f match_some

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_tuple _ ->
      fprintf ppf "tuple"
  | Match_variant (lst, _) ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_bool _ ->
      fprintf ppf "boolean"
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"

and matching_variant_case_type ppf ((c,n),_a) =
  fprintf ppf "| %a %a" constructor c expression_variable n

and option_mut ppf mut = 
  if mut then 
    fprintf ppf "[@mut]"
  else
    fprintf ppf ""

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type (type_name, te) ->
      fprintf ppf "type %a = %a" type_variable type_name type_expression te
  | Declaration_constant (name, ty_opt, i, expr) ->
      fprintf ppf "const %a = %a%a" option_type_name (name, ty_opt) expression
        expr
        option_inline i

let program ppf (p : program) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
