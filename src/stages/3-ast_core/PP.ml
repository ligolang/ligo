[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP
include Ast_PP_type(Ast_core_parameter)

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev


let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
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
  | Declaration_constant (name, ty_opt, i, expr) ->
      fprintf ppf "@[<2>const %a =@ %a%a@]" option_type_name (name, ty_opt) expression
        expr
        option_inline i

let program ppf (p : program) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
