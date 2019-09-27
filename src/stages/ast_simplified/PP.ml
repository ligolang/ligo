open Types
open PP_helpers
open Format

let list_sep_d x ppf lst = match lst with
  | [] -> ()
  | _ -> fprintf ppf "@;  @[<v>%a@]@;" (list_sep x (tag "@;")) lst

let smap_sep_d x ppf m =
  if Map.String.is_empty m
  then ()
  else fprintf ppf "@;  @[<v>%a@]@;" (smap_sep x (tag "@;")) m

let rec type_expression ppf (te:type_expression) = match te with
  | T_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d type_expression) lst
  | T_sum m -> fprintf ppf "sum[%a]" (smap_sep_d type_expression) m
  | T_record m -> fprintf ppf "record[%a]" (smap_sep_d type_expression) m
  | T_function (p, r) -> fprintf ppf "%a -> %a" type_expression p type_expression r
  | T_variable name -> fprintf ppf "%s" name
  | T_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep_d type_expression) lst

let literal ppf (l:literal) = match l with
  | Literal_unit -> fprintf ppf "Unit"
  | Literal_bool b -> fprintf ppf "%b" b
  | Literal_int n -> fprintf ppf "%d" n
  | Literal_nat n -> fprintf ppf "+%d" n
  | Literal_timestamp n -> fprintf ppf "+%d" n
  | Literal_mutez n -> fprintf ppf "%dmtz" n
  | Literal_string s -> fprintf ppf "%S" s
  | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b
  | Literal_address s -> fprintf ppf "@%S" s
  | Literal_operation _ -> fprintf ppf "Operation(...bytes)"

let rec expression ppf (e:expression) = match e.expression with
  | E_literal l -> literal ppf l
  | E_variable name -> fprintf ppf "%s" name
  | E_application (f, arg) -> fprintf ppf "(%a)@(%a)" expression f expression arg
  | E_constructor (name, ae) -> fprintf ppf "%s(%a)" name expression ae
  | E_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep_d expression) lst
  | E_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d expression) lst
  | E_accessor (ae, p) -> fprintf ppf "%a.%a" expression ae access_path p
  | E_record m -> fprintf ppf "record[%a]" (smap_sep_d expression) m
  | E_map m -> fprintf ppf "map[%a]" (list_sep_d assoc_expression) m
  | E_big_map m -> fprintf ppf "big_map[%a]" (list_sep_d assoc_expression) m
  | E_list lst -> fprintf ppf "list[%a]" (list_sep_d expression) lst
  | E_set lst -> fprintf ppf "set[%a]" (list_sep_d expression) lst
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
  | E_assign (name , path , expr) ->
    fprintf ppf "%s.%a := %a"
      name
      PP_helpers.(list_sep access (const ".")) path
      expression expr
  | E_let_in { binder ; rhs ; result } ->
      fprintf ppf "let %a = %a in %a" option_type_name binder expression rhs expression result
  | E_skip -> fprintf ppf "skip"
  | E_annotation (expr , ty) -> fprintf ppf "%a : %a" expression expr type_expression ty

and option_type_name ppf ((name , ty_opt) : string * type_expression option) =
  match ty_opt with
  | None -> fprintf ppf "%s" name
  | Some ty -> fprintf ppf "%s : %a" name type_expression ty

and assoc_expression ppf : (expr * expr) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" expression a expression b

and access ppf (a:access) =
  match a with
  | Access_tuple n -> fprintf ppf "%d" n
  | Access_record s -> fprintf ppf "%s" s
  | Access_map s -> fprintf ppf "(%a)" expression s

and access_path ppf (p:access_path) =
  fprintf ppf "%a" (list_sep access (const ".")) p

and type_annotation ppf (ta:type_expression option) = match ta with
  | None -> fprintf ppf ""
  | Some t -> type_expression ppf t

and single_record_patch ppf ((p, expr) : string * expr) =
  fprintf ppf "%s <- %a" p expression expr

and single_tuple_patch ppf ((p, expr) : int * expr) =
  fprintf ppf "%d <- %a" p expression expr

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (constructor_name * name) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %s %s -> %a" c n f a

and matching : type a . (formatter -> a -> unit) -> formatter -> a matching -> unit =
  fun f ppf m -> match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep_d string) lst f b
    | Match_variant lst ->
        fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" f match_nil hd tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" f match_none some f match_some

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_tuple _ ->
      fprintf ppf "tuple"
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_bool _ ->
      fprintf ppf "boolean"
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"

and matching_variant_case_type ppf ((c,n),_a) =
  fprintf ppf "| %s %s" c n

let declaration ppf (d:declaration) = match d with
  | Declaration_type (type_name , te) ->
      fprintf ppf "type %s = %a" type_name type_expression te
  | Declaration_constant (name , ty_opt , expr) ->
      fprintf ppf "const %a = %a" option_type_name (name , ty_opt) expression expr

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
