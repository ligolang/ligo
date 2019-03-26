module S = Ast_simplified

module SMap = Ligo_helpers.X_map.String

let list_of_smap (s:'a SMap.t) : (string * 'a) list =
  List.rev @@ SMap.fold (fun k v p -> (k, v) :: p) s []

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Constant_declaration of named_expression
  (* | Macro_declaration of macro_declaration *)

and annotated_expression = {
  expression: expression ;
  type_annotation: tv ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and tv = type_value
and ae = annotated_expression
and tv_map = type_value type_name_map
and ae_map = annotated_expression name_map

and type_value' =
  | Type_tuple of tv list
  | Type_sum of tv_map
  | Type_record of tv_map
  | Type_constant of type_name * tv list
  | Type_function of tv * tv

and type_value = {
  type_value : type_value' ;
  simplified : S.type_expression option ;
}

and lambda = {
  binder: name ;
  input_type: tv ;
  output_type: tv ;
  result: ae ;
  body: block ;
}

and expression =
  (* Base *)
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Application of ae * ae
  | Lambda of lambda
  (* Tuple *)
  | Tuple of ae list
  | Tuple_accessor of ae * int (* Access n'th tuple's element *)
  (* Sum *)
  | Constructor of name * ae (* For user defined constructors *)
  (* Record *)
  | Record of ae_map
  | Record_accessor of ae * string


and literal =
  | Unit
  | Bool of bool
  | Int of int
  | Nat of int
  | String of string
  | Bytes of bytes

and block = instruction list
and b = block

and instruction =
  | Assignment of named_expression
  | Matching of ae * matching
  | Loop of ae * b
  | Skip
  | Fail of ae

and matching =
  | Match_bool of {
      match_true : b ;
      match_false : b ;
    }
  | Match_list of {
      match_nil : b ;
      match_cons : name * name * b ;
    }
  | Match_option of {
      match_none : b ;
      match_some : name * b ;
    }
  | Match_tuple of name list * b

open! Ligo_helpers.Trace


let type_value type_value simplified = { type_value ; simplified }
let annotated_expression expression type_annotation = { expression ; type_annotation }
let get_entry (p:program) (entry : string) =
  let aux (d:declaration) =
    match d with
    | Constant_declaration {name ; annotated_expression = {expression = Lambda l ; type_annotation}} when entry = name ->
        Some (l, type_annotation)
    | _ -> None
  in
  trace_option (simple_error "no entry point with given name")
  @@ Tezos_utils.List.find_map aux p

module PP = struct
  open Format
  open Ligo_helpers.PP

  let rec type_value' ppf (tv':type_value') : unit =
    match tv' with
    | Type_tuple lst -> fprintf ppf "tuple[%a]" (list_sep type_value) lst
    | Type_sum m -> fprintf ppf "sum[%a]" (smap_sep type_value) m
    | Type_record m -> fprintf ppf "record[%a]" (smap_sep type_value) m
    | Type_function (a, b) -> fprintf ppf "%a -> %a" type_value a type_value b
    | Type_constant (c, []) -> fprintf ppf "%s" c
    | Type_constant (c, n) -> fprintf ppf "%s(%a)" c (list_sep type_value) n

  and type_value ppf (tv:type_value) : unit =
    type_value' ppf tv.type_value

  let rec annotated_expression ppf (ae:annotated_expression) : unit =
    match ae.type_annotation.simplified with
    | Some _ -> fprintf ppf "%a:%a" expression ae.expression type_value ae.type_annotation
    | _ -> expression ppf ae.expression

  and expression ppf (e:expression) : unit =
    match e with
    | Literal l -> literal ppf l
    | Constant (c, lst) -> fprintf ppf "%s(%a)" c (list_sep annotated_expression) lst
    | Constructor (c, lst) -> fprintf ppf "%s(%a)" c annotated_expression lst
    | Variable a -> fprintf ppf "%s" a
    | Application (f, arg) -> fprintf ppf "(%a) (%a)" annotated_expression f annotated_expression arg
    | Tuple lst -> fprintf ppf "tuple[%a]" (list_sep annotated_expression) lst
    | Lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_value input_type type_value output_type
          block body annotated_expression result
    | Tuple_accessor (ae, i) -> fprintf ppf "%a.%d" annotated_expression ae i
    | Record_accessor (ae, s) -> fprintf ppf "%a.%s" annotated_expression ae s
    | Record m -> fprintf ppf "record[%a]" (smap_sep annotated_expression) m

  and literal ppf (l:literal) : unit =
    match l with
    | Unit -> fprintf ppf "unit"
    | Bool b -> fprintf ppf "%b" b
    | Int n -> fprintf ppf "%d" n
    | Nat n -> fprintf ppf "%d" n
    | String s -> fprintf ppf "%s" s
    | Bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

  and block ppf (b:block) = (list_sep instruction) ppf b

  and single_record_patch ppf ((s, ae) : string * ae) =
    fprintf ppf "%s <- %a" s annotated_expression ae

  and matching ppf (m:matching) = match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep (fun ppf -> fprintf ppf "%s")) lst block b
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" block match_true block match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" block match_nil hd tl block match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" block match_none some block match_some

  and instruction ppf (i:instruction) = match i with
    | Skip -> fprintf ppf "skip"
    | Fail ae -> fprintf ppf "fail with (%a)" annotated_expression ae
    | Loop (cond, b) -> fprintf ppf "while (%a) { %a }" annotated_expression cond block b
    | Assignment {name;annotated_expression = ae} ->
        fprintf ppf "%s := %a" name annotated_expression ae
    | Matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae matching m

  let declaration ppf (d:declaration) =
    match d with
    | Constant_declaration {name ; annotated_expression = ae} ->
        fprintf ppf "const %s = %a" name annotated_expression ae

  let program ppf (p:program) =
    fprintf ppf "%a" (list_sep declaration) p

end


module Errors = struct
  let different_kinds a b =
    let title = "different kinds" in
    let full = Format.asprintf "(%a) VS (%a)" PP.type_value a PP.type_value b in
    error title full

  let different_constants a b =
    let title = "different constants" in
    let full = Format.asprintf "%s VS %s" a b in
    error title full

  let different_size_constants a b =
    let title = "constants have different sizes" in
    let full = Format.asprintf "%a VS %a" PP.type_value a PP.type_value b in
    error title full

  let different_size_tuples a b =
    let title = "tuple have different sizes" in
    let full = Format.asprintf "%a VS %a" PP.type_value a PP.type_value b in
    error title full
end
open Errors

let rec assert_type_value_eq (a, b: (type_value * type_value)) : unit result = match (a.type_value, b.type_value) with
  | Type_tuple ta, Type_tuple tb -> (
      let%bind _ =
        trace_strong (different_size_tuples a b)
        @@ Assert.assert_true List.(length ta = length tb) in
      bind_list_iter assert_type_value_eq (List.combine ta tb)
    )
  | Type_tuple _, _ -> fail @@ different_kinds a b
  | Type_constant (ca, lsta), Type_constant (cb, lstb) -> (
      let%bind _ =
        trace_strong (different_size_constants a b)
        @@ Assert.assert_true List.(length lsta = length lstb) in
      let%bind _ =
        trace_strong (different_constants ca cb)
        @@ Assert.assert_true (ca = cb) in
      trace (simple_error "constant sub-expression")
      @@ bind_list_iter assert_type_value_eq (List.combine lsta lstb)
    )
  | Type_constant _, _ -> fail @@ different_kinds a b
  | Type_sum a, Type_sum b -> (
      let a' = list_of_smap a in
      let b' = list_of_smap b in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in sum types"
          @@ (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      trace (simple_error "sum type")
      @@ bind_list_iter aux (List.combine a' b')

    )
  | Type_sum _, _ -> fail @@ different_kinds a b
  | Type_record a, Type_record b -> (
      let a' = list_of_smap a in
      let b' = list_of_smap b in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in record types"
          @@ (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      trace (simple_error "record type")
      @@ bind_list_iter aux (List.combine a' b')

    )
  | Type_record _, _ -> fail @@ different_kinds a b
  | Type_function (param, result), Type_function (param', result') ->
      let%bind _ = assert_type_value_eq (param, param') in
      let%bind _ = assert_type_value_eq (result, result') in
      ok ()
  | Type_function _, _ -> fail @@ different_kinds a b

(* No information about what made it fail *)
let type_value_eq ab = match assert_type_value_eq ab with
  | Ok _ -> true
  | _ -> false

let merge_annotation (a:type_value option) (b:type_value option) : type_value result =
  match a, b with
  | None, None -> simple_fail "no annotation"
  | Some a, None -> ok a
  | None, Some b -> ok b
  | Some a, Some b ->
      let%bind _ = assert_type_value_eq (a, b) in
      match a.simplified, b.simplified with
      | _, None -> ok a
      | None, Some _ -> ok b
      | _ -> simple_fail "both have simplified ASTs"

module Combinators = struct

  let t_bool s : type_value = type_value (Type_constant ("bool", [])) s
  let simplify_t_bool s = t_bool (Some s)
  let make_t_bool = t_bool None

  let t_string s : type_value = type_value (Type_constant ("string", [])) s
  let simplify_t_string s = t_string (Some s)
  let make_t_string = t_string None

  let t_bytes s : type_value = type_value (Type_constant ("bytes", [])) s
  let simplify_t_bytes s = t_bytes (Some s)
  let make_t_bytes = t_bytes None

  let t_int s : type_value = type_value (Type_constant ("int", [])) s
  let simplify_t_int s = t_int (Some s)
  let make_t_int = t_int None

  let t_unit s : type_value = type_value (Type_constant ("unit", [])) s
  let simplify_t_unit s = t_unit (Some s)
  let make_t_unit = t_unit None

  let t_tuple lst s : type_value = type_value (Type_tuple lst) s
  let simplify_t_tuple lst s = t_tuple lst (Some s)
  let make_t_tuple lst = t_tuple lst None

  let t_record m s : type_value = type_value (Type_record m) s
  let make_t_ez_record (lst:(string * type_value) list) : type_value =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    type_value (Type_record map) None

  let make_t_record m = t_record m None

  let t_sum m s : type_value = type_value (Type_sum m) s
  let make_t_sum m = t_sum m None

  let t_function (param, result) s : type_value = type_value (Type_function (param, result)) s
  let make_t_function f = t_function f None

  let get_annotation (x:annotated_expression) = x.type_annotation

  let get_t_bool (t:type_value) : unit result = match t.type_value with
    | Type_constant ("bool", []) -> ok ()
    | _ -> simple_fail "not a bool"

  let get_t_option (t:type_value) : type_value result = match t.type_value with
    | Type_constant ("option", [o]) -> ok o
    | _ -> simple_fail "not a option"

  let get_t_list (t:type_value) : type_value result = match t.type_value with
    | Type_constant ("list", [o]) -> ok o
    | _ -> simple_fail "not a list"

  let get_t_tuple (t:type_value) : type_value list result = match t.type_value with
    | Type_tuple lst -> ok lst
    | _ -> simple_fail "not a tuple"

  let get_t_sum (t:type_value) : type_value SMap.t result = match t.type_value with
    | Type_sum m -> ok m
    | _ -> simple_fail "not a sum"

  let get_t_record (t:type_value) : type_value SMap.t result = match t.type_value with
    | Type_record m -> ok m
    | _ -> simple_fail "not a record"

  let record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Record map

  let int n : expression = Literal (Int n)

  let a_int n = annotated_expression (int n) make_t_int

  let get_a_int (t:annotated_expression) =
    match t.expression with
    | Literal (Int n) -> ok n
    | _ -> simple_fail "not an int"
end
