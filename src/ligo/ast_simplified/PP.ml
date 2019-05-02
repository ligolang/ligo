open Types
open PP_helpers
open Format

let list_sep_d x = list_sep x (const " , ")
let smap_sep_d x = smap_sep x (const " , ")

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
  | Literal_tez n -> fprintf ppf "%dtz" n
  | Literal_string s -> fprintf ppf "%S" s
  | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

let rec expression ppf (e:expression) = match e with
  | E_literal l -> literal ppf l
  | E_variable name -> fprintf ppf "%s" name
  | E_application (f, arg) -> fprintf ppf "(%a)@(%a)" annotated_expression f annotated_expression arg
  | E_constructor (name, ae) -> fprintf ppf "%s(%a)" name annotated_expression ae
  | E_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep_d annotated_expression) lst
  | E_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d annotated_expression) lst
  | E_accessor (ae, p) -> fprintf ppf "%a.%a" annotated_expression ae access_path p
  | E_record m -> fprintf ppf "record[%a]" (smap_sep_d annotated_expression) m
  | E_map m -> fprintf ppf "map[%a]" (list_sep_d assoc_annotated_expression) m
  | E_list lst -> fprintf ppf "list[%a]" (list_sep_d annotated_expression) lst
  | E_look_up (ds, ind) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression ind
  | E_lambda {binder;input_type;output_type;result;body} ->
      fprintf ppf "lambda (%s:%a) : %a {@;  @[<v>%a@]@;} return %a"
        binder type_expression input_type type_expression output_type
        block body annotated_expression result
  | E_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m
  | E_failwith ae ->
      fprintf ppf "failwith %a" annotated_expression ae

and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

and access ppf (a:access) =
  match a with
  | Access_tuple n -> fprintf ppf "%d" n
  | Access_record s -> fprintf ppf "%s" s
  | Access_map s -> fprintf ppf "(%a)" annotated_expression s

and access_path ppf (p:access_path) =
  fprintf ppf "%a" (list_sep access (const ".")) p

and type_annotation ppf (ta:type_expression option) = match ta with
  | None -> fprintf ppf ""
  | Some t -> type_expression ppf t

and annotated_expression ppf (ae:annotated_expression) = match ae.type_annotation with
  | None -> fprintf ppf "%a" expression ae.expression
  | Some t -> fprintf ppf "(%a) : %a" expression ae.expression type_expression t

and value : _ -> value -> unit = fun x -> annotated_expression x

and block ppf (b:block) = (list_sep instruction (tag "@;")) ppf b

and single_record_patch ppf ((p, ae) : string * ae) =
  fprintf ppf "%s <- %a" p annotated_expression ae

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

and instruction ppf (i:instruction) = match i with
  | I_skip -> fprintf ppf "skip"
  | I_do ae -> fprintf ppf "do %a" annotated_expression ae
  | I_record_patch (name, path, lst) -> fprintf ppf "%s.%a[%a]" name access_path path (list_sep_d single_record_patch) lst
  | I_loop (cond, b) -> fprintf ppf "while (%a) { %a }" annotated_expression cond block b
  | I_assignment {name;annotated_expression = ae} ->
      fprintf ppf "%s := %a" name annotated_expression ae
  | I_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching block) m

let declaration ppf (d:declaration) = match d with
  | Declaration_type {type_name ; type_expression = te} ->
      fprintf ppf "type %s = %a" type_name type_expression te
  | Declaration_constant {name ; annotated_expression = ae} ->
      fprintf ppf "const %s = %a" name annotated_expression ae

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
