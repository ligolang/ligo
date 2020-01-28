[@@@coverage exclude_file]
open Types
open Format
open PP_helpers
include Stage_common.PP

let list_sep_d x = list_sep x (const " , ")


let rec type_value' ppf (tv':type_value type_expression') : unit =
  type_expression' type_value ppf tv'

and type_value ppf (tv:type_value) : unit =
  type_value' ppf tv.type_value'

let rec annotated_expression ppf (ae:annotated_expression) : unit =
  match ae.type_annotation.simplified with
  | Some _ -> fprintf ppf "@[<v>%a:%a@]" expression ae.expression type_value ae.type_annotation
  | _ -> fprintf ppf "@[<v>%a@]" expression ae.expression

and lambda ppf l =
  let ({ binder ; body } : lambda) = l in
  fprintf ppf "lambda (%a) -> %a"
    name binder 
    annotated_expression body

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and expression ppf (e:expression) : unit =
  match e with
  | E_literal l -> Stage_common.PP.literal ppf l
  | E_constant (b, lst) -> fprintf ppf "%a(%a)" constant b (list_sep_d annotated_expression) lst
  | E_constructor (c, lst) -> fprintf ppf "%a(%a)" constructor c annotated_expression lst
  | E_variable a -> fprintf ppf "%a" name a
  | E_application (f, arg) -> fprintf ppf "(%a) (%a)" annotated_expression f annotated_expression arg
  | E_lambda l -> fprintf ppf "%a" lambda l
  | E_tuple_accessor (ae, i) -> fprintf ppf "%a.%d" annotated_expression ae i
  | E_record_accessor (ae, l) -> fprintf ppf "%a.%a" annotated_expression ae label l
  | E_record_update (ae, (path,expr)) -> fprintf ppf "%a with record[%a=%a]" annotated_expression ae Stage_common.PP.label path annotated_expression expr
  | E_tuple lst -> fprintf ppf "tuple[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) lst
  | E_record m -> fprintf ppf "record[%a]" (lmap_sep annotated_expression (const " , ")) m
  | E_map m -> fprintf ppf "map[@;  @[<v>%a@]@;]" (list_sep assoc_annotated_expression (tag ",@;")) m
  | E_big_map m -> fprintf ppf "big_map[@;  @[<v>%a@]@;]" (list_sep assoc_annotated_expression (tag ",@;")) m
  | E_list m -> fprintf ppf "list[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) m
  | E_set m -> fprintf ppf "set[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) m
  | E_look_up (ds, i) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression i
  | E_matching (ae, m) ->
      fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m
  | E_sequence (a , b) -> fprintf ppf "%a ; %a" annotated_expression a annotated_expression b
  | E_loop (expr , body) -> fprintf ppf "while %a { %a }" annotated_expression expr annotated_expression body
  | E_assign (name , path , expr) ->
    fprintf ppf "%a.%a := %a"
      Stage_common.PP.name name.type_name
      PP_helpers.(list_sep pre_access (const ".")) path
      annotated_expression expr
  | E_let_in { binder; rhs; result; inline } ->
    fprintf ppf "let %a = %a%a in %a" name binder annotated_expression rhs option_inline inline annotated_expression result

and value ppf v = annotated_expression ppf v

and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

and single_record_patch ppf ((s, ae) : string * ae) =
  fprintf ppf "%s <- %a" s annotated_expression ae

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (constructor * expression_variable) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a -> %a" constructor c name n f a

and matching : type a . (formatter -> a -> unit) -> _ -> (a, 'var) matching -> unit = fun f ppf m -> match m with
  | Match_tuple ((lst, b),_) ->
      fprintf ppf "let (%a) = %a" (list_sep_d Stage_common.PP.name) lst f b
  | Match_variant (lst, _) ->
      fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
  | Match_bool {match_true ; match_false} ->
      fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
  | Match_list {match_nil ; match_cons = (hd_name, tl_name, match_cons, _)} ->
      fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil Stage_common.PP.name hd_name Stage_common.PP.name tl_name f match_cons
  | Match_option {match_none ; match_some = (some, match_some, _)} ->
      fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none name some f match_some

and pre_access ppf (a:access) = match a with
  | Access_record n -> fprintf ppf ".%s" n
  | Access_tuple i -> fprintf ppf ".%d" i

let declaration ppf (d:declaration) =
  match d with
  | Declaration_constant ({name ; annotated_expression = ae} , inline, _) ->
      fprintf ppf "const %a = %a%a" Stage_common.PP.name name annotated_expression ae option_inline inline

let program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) (List.map Location.unwrap p)
