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
  let {binder;input_type;output_type;result;body} = l in
  fprintf ppf "lambda (%s:%a) : %a {@;  @[<v>%a@]@;} return %a"
    binder type_value input_type type_value output_type
    block body annotated_expression result

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
  | E_list m -> fprintf ppf "list[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) m
  | E_look_up (ds, i) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression i
  | E_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m
  | E_failwith ae -> fprintf ppf "failwith %a" annotated_expression ae

and value ppf v = annotated_expression ppf v

and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

and literal ppf (l:literal) : unit =
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_bool b -> fprintf ppf "%b" b
  | Literal_int n -> fprintf ppf "%d" n
  | Literal_nat n -> fprintf ppf "+%d" n
  | Literal_tez n -> fprintf ppf "%dtz" n
  | Literal_string s -> fprintf ppf "%s" s
  | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

and block ppf (b:block) = (list_sep instruction (tag "@;")) ppf b

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
  | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
      fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" f match_nil hd tl f match_cons
  | Match_option {match_none ; match_some = (some, match_some)} ->
      fprintf ppf "| None -> %a @.| Some %s -> %a" f match_none (fst some) f match_some

and pre_access ppf (a:access) = match a with
  | Access_record n -> fprintf ppf ".%s" n
  | Access_tuple i -> fprintf ppf ".%d" i
  | Access_map n -> fprintf ppf ".%a" annotated_expression n

and instruction ppf (i:instruction) = match i with
  | I_skip -> fprintf ppf "skip"
  | I_do ae -> fprintf ppf "do %a" annotated_expression ae
  | I_loop (cond, b) -> fprintf ppf "while (%a) {@;  @[<v>%a@]@;}" annotated_expression cond block b
  | I_declaration {name;annotated_expression = ae} ->
      fprintf ppf "let %s = %a" name annotated_expression ae
  | I_assignment {name;annotated_expression = ae} ->
      fprintf ppf "%s := %a" name annotated_expression ae
  | I_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching block) m
  | I_patch (s, p, e) ->
      fprintf ppf "%s%a := %a"
        s.type_name (fun ppf -> List.iter (pre_access ppf)) p
        annotated_expression e

let declaration ppf (d:declaration) =
  match d with
  | Declaration_constant ({name ; annotated_expression = ae} , _) ->
      fprintf ppf "const %s = %a" name annotated_expression ae

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
