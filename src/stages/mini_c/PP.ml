open Simple_utils.PP_helpers
open Types
open Format

let list_sep_d x = list_sep x (const " , ")

let space_sep ppf () = fprintf ppf " "

let lr = fun ppf -> function `Left -> fprintf ppf "L" | `Right -> fprintf ppf "R"

let type_base ppf : type_base -> _ = function
  | Base_unit -> fprintf ppf "unit"
  | Base_void -> fprintf ppf "void"
  | Base_bool -> fprintf ppf "bool"
  | Base_int -> fprintf ppf "int"
  | Base_nat -> fprintf ppf "nat"
  | Base_tez -> fprintf ppf "tez"
  | Base_string -> fprintf ppf "string"
  | Base_address -> fprintf ppf "address"
  | Base_timestamp -> fprintf ppf "timestamp"
  | Base_bytes -> fprintf ppf "bytes"
  | Base_operation -> fprintf ppf "operation"

let rec type_ ppf : type_value -> _ = function
  | T_or(a, b) -> fprintf ppf "(%a) | (%a)" annotated a annotated b
  | T_pair(a, b) -> fprintf ppf "(%a) & (%a)" annotated a annotated b
  | T_base b -> type_base ppf b
  | T_function(a, b) -> fprintf ppf "(%a) -> (%a)" type_ a type_ b
  | T_map(k, v) -> fprintf ppf "map(%a -> %a)" type_ k type_ v
  | T_big_map(k, v) -> fprintf ppf "big_map(%a -> %a)" type_ k type_ v
  | T_list(t) -> fprintf ppf "list(%a)" type_ t
  | T_set(t) -> fprintf ppf "set(%a)" type_ t
  | T_option(o) -> fprintf ppf "option(%a)" type_ o
  | T_contract(t) -> fprintf ppf "contract(%a)" type_ t
  | T_deep_closure(c, arg, ret) ->
      fprintf ppf "[%a](%a)->(%a)"
        environment c
        type_ arg type_ ret

and annotated ppf : type_value annotated -> _ = function
  | (Some ann, a) -> fprintf ppf "(%a %%%s)" type_ a ann
  | (None, a) -> type_ ppf a

and environment_element ppf ((s, tv) : environment_element) =
  Format.fprintf ppf "%s : %a" s type_ tv

and environment ppf (x:environment) =
  fprintf ppf "Env[%a]" (list_sep_d environment_element) x

let rec value ppf : value -> unit = function
  | D_bool b -> fprintf ppf "%b" b
  | D_operation _ -> fprintf ppf "operation[...bytes]"
  | D_int n -> fprintf ppf "%d" n
  | D_nat n -> fprintf ppf "+%d" n
  | D_timestamp n -> fprintf ppf "+%d" n
  | D_mutez n -> fprintf ppf "%dmtz" n
  | D_unit -> fprintf ppf "unit"
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes x ->
     let (`Hex hex) = Hex.of_bytes x in
     fprintf ppf "0x%s" hex
  | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
  | D_left a -> fprintf ppf "L(%a)" value a
  | D_right b -> fprintf ppf "R(%a)" value b
  | D_function x -> function_ ppf x
  | D_none -> fprintf ppf "None"
  | D_some s -> fprintf ppf "Some (%a)" value s
  | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m
  | D_big_map m -> fprintf ppf "Big_map[%a]" (list_sep_d value_assoc) m
  | D_list lst -> fprintf ppf "List[%a]" (list_sep_d value) lst
  | D_set lst -> fprintf ppf "Set[%a]" (list_sep_d value) lst

and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" value a value b

and expression' ppf (e:expression') = match e with
  | E_skip -> fprintf ppf "skip"
  | E_closure x -> fprintf ppf "C(%a)" function_ x
  | E_variable v -> fprintf ppf "V(%s)" v
  | E_application(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b
  | E_constant(p, lst) -> fprintf ppf "%s %a" p (pp_print_list ~pp_sep:space_sep expression) lst
  | E_literal v -> fprintf ppf "L(%a)" value v
  | E_make_empty_map _ -> fprintf ppf "map[]"
  | E_make_empty_list _ -> fprintf ppf "list[]"
  | E_make_empty_set _ -> fprintf ppf "set[]"
  | E_make_none _ -> fprintf ppf "none"
  | E_if_bool (c, a, b) -> fprintf ppf "%a ? %a : %a" expression c expression a expression b
  | E_if_none (c, n, ((name, _) , s)) -> fprintf ppf "%a ?? %a : %s -> %a" expression c expression n name expression s
  | E_if_cons (c, n, (((hd_name, _) , (tl_name, _)) , cons)) -> fprintf ppf "%a ?? %a : (%s :: %s) -> %a" expression c expression n hd_name tl_name expression cons
  | E_if_left (c, ((name_l, _) , l), ((name_r, _) , r)) ->
      fprintf ppf "%a ?? %s -> %a : %s -> %a" expression c name_l expression l name_r expression r
  | E_sequence (a , b) -> fprintf ppf "%a ;; %a" expression a expression b
  | E_let_in ((name , _) , expr , body) ->
      fprintf ppf "let %s = %a in ( %a )" name expression expr expression body
  | E_iterator (s , ((name , _) , body) , expr) ->
      fprintf ppf "for_%s %s of %a do ( %a )" s name expression expr expression body
  | E_fold (((name , _) , body) , collection , initial) ->
      fprintf ppf "fold %a on %a with %s do ( %a )" expression collection expression initial name expression body
  | E_assignment (r , path , e) ->
      fprintf ppf "%s.%a := %a" r (list_sep lr (const ".")) path expression e
  | E_while (e , b) ->
      fprintf ppf "while (%a) %a" expression e expression b

and expression : _ -> expression -> _ = fun ppf e ->
  expression' ppf e.content

and expression_with_type : _ -> expression -> _  = fun ppf e ->
  fprintf ppf "%a : %a"
    expression' e.content
    type_ e.type_value

and function_ ppf ({binder ; body}:anon_function) =
  fprintf ppf "fun %s -> (%a)"
    binder
    expression body

and assignment ppf ((n, e):assignment) = fprintf ppf "%s = %a;" n expression e

and declaration ppf ((n, e):assignment) = fprintf ppf "let %s = %a;" n expression e

let tl_statement ppf (ass, _) = assignment ppf ass

let program ppf (p:program) =
  fprintf ppf "Program:\n---\n%a" (pp_print_list ~pp_sep:pp_print_newline tl_statement) p
