[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP

(* TODO: move to common *)
let lmap_sep value sep ppf m =
  let lst = LMap.to_kv_list m in
  let lst = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content

let list_sep_d_par f ppf lst =
  match lst with 
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_sum m -> fprintf ppf "@[<hv 4>sum[%a]@]" (lmap_sep_d type_expression) m
  | T_record m -> fprintf ppf "{%a}" (record_sep_t type_expression (const ";")) m
  | T_tuple  t -> fprintf ppf "(%a)" (list_sep_d type_expression) t
  | T_arrow  a -> fprintf ppf "%a -> %a" type_expression a.type1 type_expression a.type2
  | T_variable tv -> type_variable ppf tv
  | T_wildcard -> fprintf ppf "_"
  | T_constant (tc, lst) -> fprintf ppf "%a%a" type_constant tc (list_sep_d_par type_expression) lst
  | T_annoted (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      fprintf ppf "%a" literal l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "{%a}" (record_sep_expr expression (const ";")) m
  | E_accessor {record;path} ->
      fprintf ppf "%a.%a" expression record (list_sep accessor (const ".")) path
  | E_update {record; path; update} ->
      fprintf ppf "{ %a with %a = %a }" expression record (list_sep accessor (const ".")) path expression update
  | E_map m ->
      fprintf ppf "map[%a]" (list_sep_d assoc_expression) m
  | E_big_map m ->
      fprintf ppf "big_map[%a]" (list_sep_d assoc_expression) m
  | E_list lst ->
      fprintf ppf "list[%a]" (list_sep_d expression) lst
  | E_set lst ->
      fprintf ppf "set[%a]" (list_sep_d expression) lst
  | E_lambda {binder=(var,ty); result} ->
      fprintf ppf "lambda (%a:%a) return %a" 
        expression_variable  var
        type_expression ty
        expression result
  | E_matching {matchee; cases; _} ->
      fprintf ppf "match %a with %a"
        expression matchee (matching expression)
        cases
  | E_recursive { fun_name; fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )" 
        expression_variable fun_name 
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_let_in { let_binder ; rhs ; let_result; inline } ->    
      fprintf ppf "let %a = %a%a in %a" binder let_binder expression rhs option_inline inline expression let_result
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "{%a : %a}" expression anno_expr type_expression
        type_annotation
  | E_cond {condition; then_clause; else_clause} ->
      fprintf ppf "if %a then %a else %a"
        expression condition
        expression then_clause
        expression else_clause
  | E_sequence {expr1;expr2} ->
      fprintf ppf "{ %a; @. %a}" expression expr1 expression expr2
  | E_skip ->
      fprintf ppf "skip"
  | E_tuple t ->
      fprintf ppf "(%a)" (list_sep_d expression) t
  | E_assign {variable; access_path; expression=e} ->
      fprintf ppf "%a%a := %a" 
        expression_variable variable
        (list_sep accessor (const ".")) access_path
        expression e
  | E_for {binder; start; final; increment; body} ->
      fprintf ppf "for %a from %a to %a by %a do %a" 
        expression_variable binder
        expression start 
        expression final 
        expression increment
        expression body
  | E_for_each {binder; collection; body; _} ->
      fprintf ppf "for each %a in %a do %a" 
        option_map binder
        expression collection
        expression body
  | E_while {condition; body} ->
      fprintf ppf "while %a do %a"
        expression condition
        expression body
    
and accessor ppf a =
  match a with
    | Access_tuple i  -> fprintf ppf "%a" Z.pp_print i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "%a" expression e

and option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v 

and assoc_expression ppf : expr * expr -> unit =
 fun (a, b) -> fprintf ppf "%a -> %a" expression a expression b

and single_record_patch ppf ((p, expr) : label * expr) =
  fprintf ppf "%a <- %a" label p expression expr

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (label * expression_variable) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a -> %a" label c expression_variable n f a

and matching : (formatter -> expression -> unit) -> formatter -> matching_expr -> unit =
  fun f ppf m -> match m with
    | Match_variant lst ->
        fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil expression_variable hd expression_variable tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none expression_variable some f match_some
    | Match_tuple (lst,b) ->
        fprintf ppf "(%a) -> %a" (list_sep_d binder) lst f b
    | Match_record (lst,b) ->
        fprintf ppf "{%a} -> %a" (list_sep_d (fun ppf (a,b,_) -> fprintf ppf "%a = %a" label a expression_variable b)) lst f b
    | Match_variable (a,b) ->
        fprintf ppf "%a -> %a" binder a f b

and binder ppf (a,b) = fprintf ppf "(%a : %a)" expression_variable a type_expression b

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"
  | Match_tuple _ ->
      fprintf ppf "tuple"
  | Match_record _ ->
      fprintf ppf "record"
  | Match_variable _ ->
      fprintf ppf "variable"

and matching_variant_case_type ppf ((c,n),_a) =
  fprintf ppf "| %a %a" label c expression_variable n

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
      fprintf ppf "const %a = %a%a" 
        binder (name, ty_opt) 
        expression expr
        option_inline i

let program ppf (p : program) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
