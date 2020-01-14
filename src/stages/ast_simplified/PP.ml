[@@@coverage exclude_file]
open Types
open PP_helpers
open Format
include Stage_common.PP

let list_sep_d x ppf lst = match lst with
  | [] -> ()
  | _ -> fprintf ppf " @[<v>%a@] " (list_sep x (tag " ; ")) lst
let tuple_sep_d x ppf lst = match lst with
  | [] -> ()
  | _ -> fprintf ppf " @[<v>%a@] " (list_sep x (tag " , ")) lst

let rec te' ppf (te : type_expression type_expression') : unit =
  type_expression' type_expression ppf te

and type_expression ppf (te: type_expression) : unit =
  te' ppf te.type_expression'

let rec expression ppf (e:expression) = match e.expression with
  | E_literal l -> literal ppf l
  | E_variable n -> fprintf ppf "%a" name n
  | E_application (f, arg) -> fprintf ppf "(%a)@(%a)" expression f expression arg
  | E_constructor (c, ae) -> fprintf ppf "%a(%a)" constructor c expression ae
  | E_constant (b, lst) -> fprintf ppf "%a(%a)" constant b (list_sep_d expression) lst
  | E_tuple lst -> fprintf ppf "(%a)" (tuple_sep_d expression) lst
  | E_accessor (ae, p) -> fprintf ppf "%a.%a" expression ae access_path p
  | E_record m -> fprintf ppf "{%a}" (lrecord_sep expression (const " , ")) m
  | E_update {record; updates} -> fprintf ppf "%a with {%a}" expression record (tuple_sep_d (fun ppf (a,b) -> fprintf ppf "%a = %a" label a expression b)) updates
  | E_map m -> fprintf ppf "[%a]" (list_sep_d assoc_expression) m
  | E_big_map m -> fprintf ppf "big_map[%a]" (list_sep_d assoc_expression) m
  | E_list lst -> fprintf ppf "[%a]" (list_sep_d expression) lst
  | E_set lst -> fprintf ppf "{%a}" (list_sep_d expression) lst
  | E_look_up (ds, ind) -> fprintf ppf "(%a)[%a]" expression ds expression ind
  | E_lambda {binder;input_type;output_type;result} ->
      fprintf ppf "lambda (%a:%a) : %a return %a"
        option_type_name binder
        (PP_helpers.option type_expression) input_type (PP_helpers.option type_expression) output_type
        expression result
  | E_matching (ae, m) ->
      fprintf ppf "match %a with %a" expression ae (matching expression) m
  | E_sequence (a , b) ->
    fprintf ppf "%a ; %a"
      expression a
      expression b
  | E_loop (expr , body) ->
    fprintf ppf "%a ; %a"
      expression expr
      expression body
  | E_assign (n , path , expr) ->
    fprintf ppf "%a.%a := %a"
      name n
      PP_helpers.(list_sep access (const ".")) path
      expression expr
  | E_let_in { binder ; rhs ; result } ->
      fprintf ppf "let %a = %a in %a" option_type_name binder expression rhs expression result
  | E_skip -> fprintf ppf "skip"
  | E_ascription (expr , ty) -> fprintf ppf "%a : %a" expression expr type_expression ty

and option_type_name ppf ((n , ty_opt) : expression_variable * type_expression option) =
  match ty_opt with
  | None -> fprintf ppf "%a" name n
  | Some ty -> fprintf ppf "%a : %a" name n type_expression ty

and assoc_expression ppf : (expr * expr) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" expression a expression b

and access ppf (a:access) =
  match a with
  | Access_tuple i -> fprintf ppf "%d" i
  | Access_record l -> fprintf ppf "%s" l

and access_path ppf (p:access_path) =
  fprintf ppf "%a" (list_sep access (const ".")) p

and type_annotation ppf (ta:type_expression option) = match ta with
  | None -> fprintf ppf ""
  | Some t -> type_expression ppf t

and single_record_patch ppf ((p, expr) : string * expr) =
  fprintf ppf "%s <- %a" p expression expr

and single_tuple_patch ppf ((p, expr) : int * expr) =
  fprintf ppf "%d <- %a" p expression expr

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (constructor * expression_variable) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a -> %a" constructor c name n f a

and matching : type a . (formatter -> a -> unit) -> formatter -> (a,unit) matching -> unit =
  fun f ppf m -> match m with
    | Match_tuple ((lst, b), _) ->
        fprintf ppf "let (%a) = %a" (list_sep_d name) lst f b
    | Match_variant (lst, _) ->
        fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons, _)} ->
        fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil name hd name tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some, _)} ->
        fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none name some f match_some

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
  fprintf ppf "| %a %a" constructor c name n

let declaration ppf (d:declaration) = match d with
  | Declaration_type (type_name , te) ->
      fprintf ppf "type %a = %a" type_variable (type_name) type_expression te
  | Declaration_constant (name , ty_opt , expr) ->
      fprintf ppf "const %a = %a" option_type_name (name , ty_opt) expression expr

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
