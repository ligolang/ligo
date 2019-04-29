[@@@warning "-30"]

module S = Ast_simplified

module SMap = Map.String

type name = string
type type_name = string
type constructor_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration Location.wrap list

and declaration =
  | Declaration_constant of (named_expression * full_environment)
  (* | Macro_declaration of macro_declaration *)

and environment_element = {
  type_value : type_value ;
  source_environment : full_environment ;
}
and environment = (string * environment_element) list
and type_environment = (string * type_value) list
and small_environment = (environment * type_environment)
and full_environment = small_environment List.Ne.t

and annotated_expression = {
  expression: expression ;
  type_annotation: tv ;
  environment: full_environment ;
  dummy_field: unit ;
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
  | T_tuple of tv list
  | T_sum of tv_map
  | T_record of tv_map
  | T_constant of type_name * tv list
  | T_function of (tv * tv)

and type_value = {
  type_value' : type_value' ;
  simplified : S.type_expression option ;
}

and named_type_value = {
  type_name: name ;
  type_value : type_value ;
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
  | E_literal of literal
  | E_constant of (name * ae list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_application of (ae * ae)
  | E_lambda of lambda
  (* Tuple *)
  | E_tuple of ae list
  | E_tuple_accessor of (ae * int) (* Access n'th tuple's element *)
  (* Sum *)
  | E_constructor of (name * ae) (* For user defined constructors *)
  (* Record *)
  | E_record of ae_map
  | E_record_accessor of (ae * string)
  (* Data Structures *)
  | E_map of (ae * ae) list
  | E_list of ae list
  | E_look_up of (ae * ae)
  (* Advanced *)
  | E_matching of (ae * matching_expr)
  | E_failwith of ae

and value = annotated_expression (* todo (for refactoring) *)

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_tez of int
  | Literal_string of string
  | Literal_bytes of bytes

and block = instruction list
and b = block

and instruction =
  | I_declaration of named_expression
  | I_assignment of named_expression
  | I_matching of ae * matching_instr
  | I_loop of ae * b
  | I_do of ae
  | I_skip
  | I_patch of named_type_value * access_path * ae

and access = Ast_simplified.access

and access_path = Ast_simplified.access_path

and 'a matching =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : name * name * 'a ;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : (name * type_value) * 'a ;
    }
  | Match_tuple of (name list * 'a)
  | Match_variant of (((constructor_name * name) * 'a) list * type_value)

and matching_instr = b matching

and matching_expr = ae matching

open Trace

let get_entry (p:program) (entry : string) : annotated_expression result =
  let aux (d:declaration) =
    match d with
    | Declaration_constant ({name ; annotated_expression} , _) when entry = name -> Some annotated_expression
    | Declaration_constant _ -> None
  in
  let%bind result =
    trace_option (simple_error "no entry point with given name") @@
    Tezos_utils.List.find_map aux (List.map Location.unwrap p) in
  ok result

let get_functional_entry (p:program) (entry : string) : (lambda * type_value) result =
  let%bind entry = get_entry p entry in
  match entry.expression with
  | E_lambda l -> ok (l, entry.type_annotation)
  | _ -> simple_fail "given entry point is not functional"

