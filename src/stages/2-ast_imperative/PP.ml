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

let attributes_2 (attr: string list) : string =
  List.map (fun s -> "[@@" ^ s ^ "]") attr |> String.concat ""

let attributes_1 (attr: string list) : string =
  List.map (fun s -> "[@" ^ s ^ "]") attr |> String.concat ""


let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type; attributes; _}) =
    let attr = attributes_2 attributes in
    fprintf ppf "@[<h>%a -> %a %s@]" label k value associated_type attr
  in fprintf ppf "%a" (list_sep new_pp sep) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_sum m ->
    let s ppf = fprintf ppf "@[<hv 4>sum[%a]@]" (lmap_sep_d type_expression) in
    if m.attributes = [] then
      fprintf ppf "%a" s m.fields
    else
      let attr = attributes_1 m.attributes in
      fprintf ppf "(%a %s)" s m.fields attr
  | T_record m ->
    let r = record_sep_t type_expression (const ";") in
    if m.attributes = [] then
      fprintf ppf "{%a}" r m.fields
    else
      let attr : string = attributes_1 m.attributes in
      fprintf ppf "({%a} %s)" r m.fields attr

  | T_variable        tv -> type_variable ppf tv
  | T_tuple            t -> type_tuple    type_expression ppf t
  | T_arrow            a -> arrow         type_expression ppf a
  | T_annoted  (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str
  | T_app            app -> type_app      type_expression ppf app
  | T_module_accessor ma -> module_access type_expression ppf ma
  | T_singleton       x  -> literal       ppf             x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> literal                ppf l
  | E_variable    n -> expression_variable    ppf n
  | E_application a -> application expression ppf a
  | E_constructor c -> constructor expression ppf c
  | E_constant c ->
      fprintf ppf "%a(%a)"
        constant' (const_name c.cons_name)
        (list_sep_d expression) c.arguments
  | E_record      r -> record      expression ppf r
  | E_tuple       t -> tuple       expression ppf t
  | E_accessor    a -> accessor    expression ppf a
  | E_update      u -> update      expression ppf u
  | E_lambda      l -> lambda      expression type_expression ppf l
  | E_matching {matchee; cases; _} ->
      fprintf ppf "match %a with %a"
        expression matchee (matching expression)
        cases
  | E_recursive  r -> recursive expression type_expression ppf r
  | E_let_in    li -> let_in  expression type_expression ppf li
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_raw_code   r -> raw_code expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression ppf ma
  | E_cond       c -> cond       expression ppf c
  | E_sequence   s -> sequence   expression ppf s
  | E_skip         -> skip                  ppf ()
  | E_map        m -> map        expression ppf m
  | E_big_map    m -> big_map    expression ppf m
  | E_list       l -> lst        expression ppf l
  | E_set        s -> set        expression ppf s
  | E_assign     a -> assign     expression ppf a
  | E_for        f -> for_       expression ppf f
  | E_for_each   f -> for_each   expression ppf f
  | E_while      w -> while_     expression ppf w

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
        fprintf ppf "(%a) -> %a" (list_sep_d (binder type_expression)) lst f b
    | Match_record (lst,b) ->
        fprintf ppf "{%a} -> %a" (list_sep_d (fun ppf (a,b) -> fprintf ppf "%a = %a" label a (binder type_expression) b)) lst f b
    | Match_variable (a,b) ->
        fprintf ppf "%a -> %a" (binder type_expression) a f b


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

and attributes ppf attributes =
  let attr =
    List.map (fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat ""
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type     dt -> declaration_type                type_expression ppf dt
  | Declaration_constant dc -> declaration_constant expression type_expression ppf dc

let program ppf (p : program) = program declaration ppf p
