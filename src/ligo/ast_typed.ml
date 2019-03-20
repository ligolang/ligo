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
and e_map = expression name_map

and type_value =
  | Type_tuple of tv list
  | Type_sum of tv_map
  | Type_record of tv_map
  | Type_constant of type_name * tv list

and expression =
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Tuple of ae list
  | Constructor of name * ae list (* For user defined constructors *)
  | Lambda of {
      binder: name ;
      input_type: type_value ;
      output_type: type_value ;
      body: block ;
    }

and literal =
  | Bool of bool
  | Number of int
  | String of string
  | Bytes of bytes

and block = instruction list
and b = block

and instruction =
  | Assignment of named_expression
  | Matching of matching
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

open Ligo_helpers.Trace

let rec type_value_eq (ab: (type_value * type_value)) : unit result = match ab with
  | Type_tuple a, Type_tuple b -> (
      let%bind _ =
        Assert.assert_true ~msg:"tuples with different sizes"
        @@ List.(length a = length b) in
      bind_list_iter type_value_eq (List.combine a b)
    )
  | Type_constant (a, a'), Type_constant (b, b') -> (
      let%bind _ =
        Assert.assert_true ~msg:"constants with different sizes"
        @@ List.(length a' = length b') in
      let%bind _ =
        Assert.assert_true ~msg:"constants with different names"
        @@ (a = b) in
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
  | _ -> simple_fail "Different kinds of types"

let merge_annotation (a:type_value option) (b:type_value option) : type_value option result =
  match a, b with
  | None, None -> ok None
  | Some a, None -> ok (Some a)
  | None, Some b -> ok (Some b)
  | Some a, Some b ->
      let%bind _ = type_value_eq (a, b) in
      ok (Some a)
