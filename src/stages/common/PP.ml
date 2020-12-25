open Types
open Format
open PP_helpers
include PP_enums

let option_inline ppf inline =
  if inline then
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

let label ppf (l:label) : unit =
  let Label l = l in fprintf ppf "%s" l

let expression_variable ppf (t : expression_variable) : unit = fprintf ppf "%a" Var.pp t.wrap_content
let type_variable       ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t

and access f ppf a =
  match a with
    | Access_tuple i  -> fprintf ppf "%a" Z.pp_print i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "%a" f e

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst
let variant_sep_d x = record_sep x (tag " ,@ ")

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let layout ppf layout = match layout with
  | L_tree -> fprintf ppf "tree"
  | L_comb -> fprintf ppf "comb"

let option_layout ppf l = match l with
  | Some l -> fprintf ppf "[layout:%a]" layout l
  | None   -> fprintf ppf ""

let layout_option = option layout

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

let attributes ppf attributes =
  let attr =
    List.map (fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat ""
  in fprintf ppf "%s" attr

let module_access f ppf = fun {module_name;element} ->
  fprintf ppf "%s.%a" module_name f element
(* Types *)

let type_app type_expression ppf ({type_operator ; arguments}: 'a type_app) : unit =
  fprintf ppf "%a%a" type_variable type_operator (list_sep_d_par type_expression) arguments

let sum type_expression ppf = fun sum ->
  fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression) sum

let type_record type_expression ppf = fun record ->
  fprintf ppf "%a" (tuple_or_record_sep_type type_expression) record

let type_tuple type_expression ppf = fun tuple ->
  fprintf ppf "(%a)" (list_sep_d type_expression) tuple

let arrow type_expression ppf = fun {type1;type2} ->
  fprintf ppf "%a -> %a" type_expression type1 type_expression type2

let wildcard ppf = fun () ->
  fprintf ppf "_"

(* Expressions *)

let binder type_expression ppf {var;ascr} =
  match ascr with
  | None ->
      fprintf ppf "%a" expression_variable var
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable var type_expression ty

let application expression ppf = fun {lamb;args} ->
  fprintf ppf "@[<hv>(%a)@@(%a)@]" expression lamb expression args

let constructor expression ppf = fun {constructor;element} ->
  fprintf ppf "@[%a(%a)@]" label constructor expression element

let constant expression ppf = fun {cons_name;arguments} ->
  fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant' cons_name (list_sep_d expression) arguments

let record expression ppf = fun r ->
  fprintf ppf "%a" (tuple_or_record_sep_expr expression) r

let record_accessor expression ppf = fun ({record;path}: _ record_accessor) ->
  fprintf ppf "@[%a.%a@]" expression record label path

let record_update expression ppf = fun {record; path; update} ->
  fprintf ppf "@[{ %a@;<1 2>with@;<1 2>{ %a = %a } }@]" expression record label path expression update

let tuple expression ppf = fun t ->
  fprintf ppf "(%a)" (list_sep_d expression) t

let accessor expression ppf = fun ({record;path}: _ accessor) ->
  fprintf ppf "%a.%a" expression record (list_sep (access expression) (const ".")) path

let update expression ppf = fun ({record; path; update}:_ update) ->
  fprintf ppf "{ %a with %a = %a }" expression record (list_sep (access expression) (const ".")) path expression update

let raw_code expression ppf = fun {language; code} ->
  fprintf ppf "[%%%s %a]" language expression code

let option_type_expression type_expression ppf = function
  None -> fprintf ppf ""
| Some te -> fprintf ppf " : %a" type_expression te

let lambda expression type_expression ppf = fun {binder=b; output_type; result} ->
  fprintf ppf "lambda (%a)%a return %a"
    (binder type_expression) b
    (option_type_expression type_expression) output_type
    expression result

let _option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v


and single_record_patch expression ppf ((p, expr) : label * 'expr) =
  fprintf ppf "%a <- %a" label p expression expr
let recursive expression type_expression ppf = fun { fun_name;fun_type; lambda=l} ->
  fprintf ppf "rec (%a:%a => %a )"
    expression_variable fun_name
    type_expression fun_type
    (lambda expression type_expression) l

let let_in expression type_expression ppf = fun {let_binder; rhs; let_result; attributes=attr} ->
  fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]"
    (binder type_expression) let_binder
    expression rhs
    attributes attr
    expression let_result

let type_in expression type_expression ppf = fun {type_binder; rhs; let_result;} ->
  fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
    type_variable type_binder
    type_expression rhs
    expression let_result

let ascription expression type_expression ppf = fun {anno_expr; type_annotation} ->
  fprintf ppf "%a : %a"
    expression anno_expr
    type_expression type_annotation

let cond expression ppf = fun {condition; then_clause; else_clause} ->
  fprintf ppf "if %a then %a else %a"
    expression condition
    expression then_clause
    expression else_clause

let sequence expression ppf = fun {expr1;expr2} ->
  fprintf ppf "{ %a; @. %a}"
    expression expr1
    expression expr2

let skip ppf = fun () ->
  fprintf ppf "skip"

let assoc_expression expression ppf : 'exp * 'exp -> unit =
 fun (a, b) -> fprintf ppf "%a -> %a" expression a expression b

let map expression ppf = fun m ->
  fprintf ppf "map[%a]" (list_sep_d (assoc_expression expression)) m

let big_map expression ppf = fun m ->
  fprintf ppf "big_map[%a]" (list_sep_d (assoc_expression expression)) m

let lst expression ppf = fun lst ->
  fprintf ppf "list[%a]" (list_sep_d expression) lst

let set expression ppf = fun set ->
  fprintf ppf "set[%a]" (list_sep_d expression) set

let assign expression ppf = fun {variable; access_path; expression=e} ->
  fprintf ppf "%a%a := %a"
    expression_variable variable
    (list_sep (access expression) (const ".")) access_path
    expression e

let for_ expression ppf = fun {binder; start; final; incr; f_body} ->
  fprintf ppf "for %a from %a to %a by %a do %a"
    expression_variable binder
    expression start
    expression final
    expression incr
    expression f_body

let option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v

let for_each expression ppf = fun {fe_binder; collection; fe_body; _} ->
  fprintf ppf "for each %a in %a do %a"
    option_map fe_binder
    expression collection
    expression fe_body

let while_ expression ppf = fun {cond; body} ->
  fprintf ppf "while %a do %a"
    expression cond
    expression body


(* Declaration *)
let declaration_type type_expression ppf = fun {type_binder;type_expr} ->
  fprintf ppf "@[<2>type %a =@ %a@]" type_variable type_binder type_expression type_expr

let declaration_constant expression type_expression ppf = fun {binder=binder'; attr ; expr} ->
  fprintf ppf "@[<2>const %a =@ %a%a@]"
    (binder type_expression) binder'
    expression expr
    attributes attr

let program declaration ppf = fun p ->
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
