[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP

let cmap_sep value sep ppf m =
  let lst = CMap.to_kv_list m in
  let lst = List.sort (fun (Constructor a,_) (Constructor b,_) -> String.compare a b) lst in
  let new_pp ppf (k, ({ctor_type=v;_}:ctor_content)) = fprintf ppf "@[<h>%a -> %a@]" constructor k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let cmap_sep_d x = cmap_sep x (tag " ,@ ")

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, ({field_type=v;_}:field_content)) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev

let rec type_expression' :
        (formatter -> type_expression -> unit)
    -> formatter
    -> type_expression
    -> unit =
  fun f ppf te ->
  match te.type_content with
  | T_sum m -> fprintf ppf "sum[%a]" (cmap_sep_d f) m
  | T_record m -> fprintf ppf "{%a}" (record_sep_t f (const ";")) m
  | T_tuple t -> fprintf ppf "(%a)" (list_sep_d f) t
  | T_arrow a -> fprintf ppf "%a -> %a" f a.type1 f a.type2
  | T_variable tv -> type_variable ppf tv
  | T_constant tc -> type_constant ppf tc
  | T_operator to_ -> type_operator f ppf to_
  | T_annoted (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str

and type_expression ppf (te : type_expression) : unit =
  type_expression' type_expression ppf te

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
  fprintf ppf "(TO_%s)" s

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" constructor c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "{%a}" (record_sep expression (const ";")) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with %a = %a }" expression record label path expression update
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
      fprintf ppf "lambda (%a:%a) : %a return %a" 
        expression_variable binder
        (PP_helpers.option type_expression)
        input_type
        (PP_helpers.option type_expression)
        output_type expression result
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
      fprintf ppf "let %a = %a%a in %a" option_type_name let_binder expression rhs option_inline inline expression let_result
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "%a : %a" expression anno_expr type_expression
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
  | E_tuple_accessor ta ->
      fprintf ppf "%a.%d" expression ta.tuple ta.path
  | E_tuple_update {tuple; path; update} ->
      fprintf ppf "{ %a with %d = %a }" expression tuple path expression update
  | E_assign {variable; access_path; expression=e} ->
      fprintf ppf "%a%a := %a" 
        expression_variable variable
        (list_sep (fun ppf a -> fprintf ppf ".%a" accessor a) (fun ppf () -> fprintf ppf "")) access_path
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
    | Access_tuple i  -> fprintf ppf "%d" i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "%a" expression e

and option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v 

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
