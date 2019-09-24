open Types
open Format
open PP_helpers

let list_sep_d x = list_sep x (const " , ")
let smap_sep_d x = smap_sep x (const " , ")


let rec type_value' ppf (tv':type_value') : unit =
  match tv' with
  | T_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d type_value) lst
  | T_sum m -> fprintf ppf "sum[%a]" (smap_sep_d type_value) m
  | T_record m -> fprintf ppf "record[%a]" (smap_sep_d type_value) m
  | T_function (a, b) -> fprintf ppf "%a -> %a" type_value a type_value b
  | T_constant (c, []) -> fprintf ppf "%s" c
  | T_constant (c, n) -> fprintf ppf "%s(%a)" c (list_sep_d type_value) n

and type_value ppf (tv:type_value) : unit =
  type_value' ppf tv.type_value'

let rec annotated_expression ppf (ae:annotated_expression) : unit =
  match ae.type_annotation.simplified with
  | Some _ -> fprintf ppf "@[<v>%a:%a@]" expression ae.expression type_value ae.type_annotation
  | _ -> fprintf ppf "@[<v>%a@]" expression ae.expression

and lambda ppf l =
  let ({ binder ; body } : lambda) = l in
  fprintf ppf "lambda (%s) -> %a"
    binder annotated_expression body

and expression ppf (e:expression) : unit =
  match e with
  | E_literal l -> literal ppf l
  | E_constant (c, lst) -> fprintf ppf "%s(%a)" c (list_sep_d annotated_expression) lst
  | E_constructor (c, lst) -> fprintf ppf "%s(%a)" c annotated_expression lst
  | E_variable a -> fprintf ppf "%s" a
  | E_application (f, arg) -> fprintf ppf "(%a) (%a)" annotated_expression f annotated_expression arg
  | E_lambda l -> fprintf ppf "%a" lambda l
  | E_tuple_accessor (ae, i) -> fprintf ppf "%a.%d" annotated_expression ae i
  | E_record_accessor (ae, s) -> fprintf ppf "%a.%s" annotated_expression ae s
  | E_tuple lst -> fprintf ppf "tuple[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) lst
  | E_record m -> fprintf ppf "record[%a]" (smap_sep_d annotated_expression) m
  | E_map m -> fprintf ppf "map[@;  @[<v>%a@]@;]" (list_sep assoc_annotated_expression (tag ",@;")) m
  | E_big_map m -> fprintf ppf "big_map[@;  @[<v>%a@]@;]" (list_sep assoc_annotated_expression (tag ",@;")) m
  | E_list m -> fprintf ppf "list[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) m
  | E_set m -> fprintf ppf "set[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) m
  | E_look_up (ds, i) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression i
  | E_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m
  | E_failwith ae -> fprintf ppf "failwith %a" annotated_expression ae
  | E_sequence (a , b) -> fprintf ppf "%a ; %a" annotated_expression a annotated_expression b
  | E_loop (expr , body) -> fprintf ppf "while %a { %a }" annotated_expression expr annotated_expression body
  | E_assign (name , path , expr) ->
    fprintf ppf "%s.%a := %a"
      name.type_name
      PP_helpers.(list_sep pre_access (const ".")) path
      annotated_expression expr
  | E_let_in { binder; rhs; result } ->
      fprintf ppf "let %s = %a in %a" binder annotated_expression rhs annotated_expression result

and value ppf v = annotated_expression ppf v

and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

and literal ppf (l:literal) : unit =
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_bool b -> fprintf ppf "%b" b
  | Literal_int n -> fprintf ppf "%d" n
  | Literal_nat n -> fprintf ppf "+%d" n
  | Literal_timestamp n -> fprintf ppf "+%d" n
  | Literal_mutez n -> fprintf ppf "%dmtz" n
  | Literal_string s -> fprintf ppf "%s" s
  | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b
  | Literal_address s -> fprintf ppf "@%s" s
  | Literal_operation _ -> fprintf ppf "Operation(...bytes)"

and single_record_patch ppf ((s, ae) : string * ae) =
  fprintf ppf "%s <- %a" s annotated_expression ae

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (constructor_name * name) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %s %s -> %a" c n f a

and matching : type a . (formatter -> a -> unit) -> _ -> a matching -> unit = fun f ppf m -> match m with
  | Match_tuple (lst, b) ->
      fprintf ppf "let (%a) = %a" (list_sep_d (fun ppf -> fprintf ppf "%s")) lst f b
  | Match_variant (lst , _) ->
      fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
  | Match_bool {match_true ; match_false} ->
      fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
  | Match_list {match_nil ; match_cons = (((hd_name , _), (tl_name , _)), match_cons)} ->
      fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" f match_nil hd_name tl_name f match_cons
  | Match_option {match_none ; match_some = (some, match_some)} ->
      fprintf ppf "| None -> %a @.| Some %s -> %a" f match_none (fst some) f match_some

and pre_access ppf (a:access) = match a with
  | Access_record n -> fprintf ppf ".%s" n
  | Access_tuple i -> fprintf ppf ".%d" i
  | Access_map n -> fprintf ppf ".%a" annotated_expression n

let declaration ppf (d:declaration) =
  match d with
  | Declaration_constant ({name ; annotated_expression = ae} , _) ->
      fprintf ppf "const %s = %a" name annotated_expression ae

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
