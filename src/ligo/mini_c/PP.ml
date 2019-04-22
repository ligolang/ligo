open PP_helpers
open Types
open Format

let list_sep_d x = list_sep x (const " , ")

let space_sep ppf () = fprintf ppf " "

let type_base ppf : type_base -> _ = function
  | Base_unit -> fprintf ppf "unit"
  | Base_bool -> fprintf ppf "bool"
  | Base_int -> fprintf ppf "int"
  | Base_nat -> fprintf ppf "nat"
  | Base_string -> fprintf ppf "string"
  | Base_bytes -> fprintf ppf "bytes"
  | Base_operation -> fprintf ppf "operation"

let rec type_ ppf : type_value -> _ = function
  | T_or(a, b) -> fprintf ppf "(%a) | (%a)" type_ a type_ b
  | T_pair(a, b) -> fprintf ppf "(%a) & (%a)" type_ a type_ b
  | T_base b -> type_base ppf b
  | T_function(a, b) -> fprintf ppf "(%a) -> (%a)" type_ a type_ b
  | T_map(k, v) -> fprintf ppf "map(%a -> %a)" type_ k type_ v
  | T_list(t) -> fprintf ppf "list(%a)" type_ t
  | T_option(o) -> fprintf ppf "option(%a)" type_ o
  | T_shallow_closure(_, a, b) -> fprintf ppf "[big_closure](%a) -> (%a)" type_ a type_ b
  | T_deep_closure(c, arg, ret) ->
      fprintf ppf "[%a](%a)->(%a)"
        environment_small c
        type_ arg type_ ret

and environment_element ppf ((s, tv) : environment_element) =
  Format.fprintf ppf "%s : %a" s type_ tv

and environment_small' ppf e' = let open Append_tree in
  let lst = to_list' e' in
  fprintf ppf "S[%a]" (list_sep_d environment_element) lst

and environment_small ppf e = let open Append_tree in
  let lst = to_list e in
  fprintf ppf "S[%a]" (list_sep_d environment_element) lst

let environment ppf (x:environment) =
  fprintf ppf "Env[%a]" (list_sep_d environment_small) x

let rec value ppf : value -> unit = function
  | D_bool b -> fprintf ppf "%b" b
  | D_int n -> fprintf ppf "%d" n
  | D_nat n -> fprintf ppf "%d" n
  | D_unit -> fprintf ppf " "
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes _ -> fprintf ppf "[bytes]"
  | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
  | D_left a -> fprintf ppf "L(%a)" value a
  | D_right b -> fprintf ppf "R(%a)" value b
  | D_function x -> function_ ppf x.content
  | D_none -> fprintf ppf "None"
  | D_some s -> fprintf ppf "Some (%a)" value s
  | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m
  | D_list lst -> fprintf ppf "List[%a]" (list_sep_d value) lst

and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" value a value b

and expression' ppf (e:expression') = match e with
  | E_variable v -> fprintf ppf "%s" v
  | E_application(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b
  | E_constant(p, lst) -> fprintf ppf "%s %a" p (pp_print_list ~pp_sep:space_sep expression) lst
  | E_literal v -> fprintf ppf "%a" value v
  | E_function c -> function_ ppf c
  | E_empty_map _ -> fprintf ppf "map[]"
  | E_empty_list _ -> fprintf ppf "list[]"
  | E_make_none _ -> fprintf ppf "none"
  | E_Cond (c, a, b) -> fprintf ppf "%a ? %a : %a" expression c expression a expression b
  | E_if_none (c, n, ((name, _) , s)) -> fprintf ppf "%a ?? %a : %s -> %a" expression c expression n name expression s
  | E_if_left (c, ((name_l, _) , l), ((name_r, _) , r)) ->
      fprintf ppf "%a ?? %s -> %a : %s -> %a" expression c name_l expression l name_r expression r
  | E_let_in ((name , _) , expr , body) ->
      fprintf ppf "let %s = %a in %a" name expression expr expression body

and expression : _ -> expression -> _ = fun ppf e ->
  expression' ppf e.content

and expression_with_type : _ -> expression -> _  = fun ppf e ->
  fprintf ppf "%a : %a"
    expression' e.content
    type_ e.type_value

and function_ ppf ({binder ; input ; output ; body ; result ; capture_type}:anon_function_content) =
  fprintf ppf "fun[%s] (%s:%a) : %a %a return %a"
    (match capture_type with
     | No_capture -> "quote"
     | Shallow_capture _ -> "shallow"
     | Deep_capture _ -> "deep")
    binder
    type_ input
    type_ output
    block body
    expression result

and assignment ppf ((n, e):assignment) = fprintf ppf "%s = %a;" n expression e

and declaration ppf ((n, e):assignment) = fprintf ppf "let %s = %a;" n expression e

and statement ppf ((s, _) : statement) = match s with
  | S_environment_extend -> fprintf ppf "extend"
  | S_environment_restrict -> fprintf ppf "restrict"
  | S_environment_add (name, tv) -> fprintf ppf "add %s %a" name type_ tv
  | S_declaration ass -> declaration ppf ass
  | S_assignment ass -> assignment ppf ass
  | S_cond (expr, i, e) -> fprintf ppf "if (%a) %a %a" expression expr block i block e
  | S_patch (r, path, e) ->
      let aux = fun ppf -> function `Left -> fprintf ppf ".L" | `Right -> fprintf ppf ".R" in
      fprintf ppf "%s%a := %a" r (list aux) path expression e
  | S_if_none (expr, none, ((name, _), some)) -> fprintf ppf "if_none (%a) %a %s->%a" expression expr block none name block some
  | S_while (e, b) -> fprintf ppf "while (%a) %a" expression e block b

and block ppf ((b, _):block) =
  match b with
  | [] -> fprintf ppf "{}"
  | b -> fprintf ppf "{@;  @[<v>%a@]@;}" (pp_print_list ~pp_sep:(tag "@;") statement) b

let tl_statement ppf (ass, _) = assignment ppf ass

let program ppf (p:program) =
  fprintf ppf "Program:\n---\n%a" (pp_print_list ~pp_sep:pp_print_newline tl_statement) p
