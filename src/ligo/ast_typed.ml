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

and type_value =
  | Type_tuple of tv list
  | Type_sum of tv_map
  | Type_record of tv_map
  | Type_constant of type_name * tv list
  | Type_function of tv * tv

and expression =
  (* Base *)
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Application of ae * ae
  | Lambda of {
      binder: name ;
      input_type: tv ;
      output_type: tv ;
      result: ae ;
      body: block ;
    }
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
  | Match_tuple of (name * b) list

module PP = struct
  open Format
  open Ligo_helpers.PP

  let rec type_value ppf (tv:type_value) : unit =
    match tv with
    | Type_tuple lst -> fprintf ppf "tuple[%a]" (list_sep type_value) lst
    | Type_sum m -> fprintf ppf "sum[%a]" (smap_sep type_value) m
    | Type_record m -> fprintf ppf "record[%a]" (smap_sep type_value) m
    | Type_function (a, b) -> fprintf ppf "%a -> %a" type_value a type_value b
    | Type_constant (c, []) -> fprintf ppf "%s" c
    | Type_constant (c, n) -> fprintf ppf "%s(%a)" c (list_sep type_value) n
end

open Ligo_helpers.Trace

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

let rec type_value_eq (ab: (type_value * type_value)) : unit result = match ab with
  | Type_tuple a as ta, (Type_tuple b as tb) -> (
      let%bind _ =
        trace_strong (different_size_tuples ta tb)
        @@ Assert.assert_true List.(length a = length b) in
      bind_list_iter type_value_eq (List.combine a b)
    )
  | Type_constant (a, a') as ca, (Type_constant (b, b') as cb) -> (
      let%bind _ =
        trace_strong (different_size_constants ca cb)
        @@ Assert.assert_true List.(length a' = length b') in
      let%bind _ =
        trace_strong (different_constants a b)
        @@ Assert.assert_true (a = b) in
      trace (simple_error "constant sub-expression")
      @@ bind_list_iter type_value_eq (List.combine a' b')
    )
  | Type_sum a, Type_sum b -> (
      let a' = list_of_smap a in
      let b' = list_of_smap b in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in sum types"
          @@ (ka = kb) in
        type_value_eq (va, vb)
      in
      trace (simple_error "sum type")
      @@ bind_list_iter aux (List.combine a' b')

    )
  | Type_record a, Type_record b -> (
      let a' = list_of_smap a in
      let b' = list_of_smap b in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in record types"
          @@ (ka = kb) in
        type_value_eq (va, vb)
      in
      trace (simple_error "record type")
      @@ bind_list_iter aux (List.combine a' b')

    )
  | a, b -> fail @@ different_kinds a b

let merge_annotation (a:type_value option) (b:type_value option) : type_value result =
  match a, b with
  | None, None -> simple_fail "no annotation"
  | Some a, None -> ok a
  | None, Some b -> ok b
  | Some a, Some b ->
      let%bind _ = type_value_eq (a, b) in
      ok a

let t_bool : type_value = Type_constant ("bool", [])
let t_string : type_value = Type_constant ("string", [])
let t_bytes : type_value = Type_constant ("bytes", [])
let t_int : type_value = Type_constant ("int", [])
let t_unit : type_value = Type_constant ("unit", [])

let get_annotation (x:annotated_expression) = x.type_annotation

let get_t_tuple : type_value -> type_value list result = function
  | Type_tuple lst -> ok lst
  | _ -> simple_fail "not a tuple"

let get_t_sum : type_value -> type_value SMap.t result = function
  | Type_sum m -> ok m
  | _ -> simple_fail "not a sum"

let get_t_record : type_value -> type_value SMap.t result = function
  | Type_record m -> ok m
  | _ -> simple_fail "not a record"
