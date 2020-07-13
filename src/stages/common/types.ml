type expression_
and expression_variable = expression_ Var.t Location.wrap
type type_
and type_variable = type_ Var.t

type constructor' = Constructor of string
type label = Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)


type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t


  and type_operator =
    | TC_contract
    | TC_option
    | TC_list
    | TC_set
    | TC_map
    | TC_big_map
    | TC_map_or_big_map
    | TC_michelson_pair
    | TC_michelson_or
    | TC_michelson_pair_right_comb
    | TC_michelson_pair_left_comb
    | TC_michelson_or_right_comb
    | TC_michelson_or_left_comb

include Enums_utils
include Enums

module type AST_PARAMETER_TYPE = sig
  type type_meta
end

module Ast_generic_type (PARAMETER : AST_PARAMETER_TYPE) = struct
  open PARAMETER


  type type_content =
    | T_sum of ctor_content constructor_map
    | T_record of field_content label_map
    | T_arrow of arrow
    | T_variable of type_variable
    | T_constant of type_constant
    | T_operator of (type_operator * type_expression list)

  and arrow = {type1: type_expression; type2: type_expression}
  
  and ctor_content = {ctor_type : type_expression ; michelson_annotation : string option ; ctor_decl_pos : int}

  and field_content = {field_type : type_expression ; field_annotation : string option ; field_decl_pos : int}


  and type_expression = {type_content: type_content; location: Location.t; type_meta: type_meta}

  open Trace
  let type_operator_name = function
        TC_contract                  -> "TC_contract"
      | TC_option                    -> "TC_option"
      | TC_list                      -> "TC_list"
      | TC_set                       -> "TC_set"
      | TC_map                       -> "TC_map"
      | TC_big_map                   -> "TC_big_map"
      | TC_map_or_big_map            -> "TC_map_or_big_map"
      | TC_michelson_pair            -> "TC_michelson_pair"
      | TC_michelson_or              -> "TC_michelson_or"
      | TC_michelson_pair_right_comb -> "TC_michelson_pair_right_comb" 
      | TC_michelson_pair_left_comb  -> "TC_michelson_pair_left_comb" 
      | TC_michelson_or_right_comb   -> "TC_michelson_or_right_comb" 
      | TC_michelson_or_left_comb    -> "TC_michelson_or_left_comb" 

  let type_expression'_of_string = function
    | "TC_contract" , [x]     -> ok @@ T_operator(TC_contract, [x])
    | "TC_option"   , [x]     -> ok @@ T_operator(TC_option, [x])
    | "TC_list"     , [x]     -> ok @@ T_operator(TC_list, [x])
    | "TC_set"      , [x]     -> ok @@ T_operator(TC_set, [x])
    | "TC_map"      , [x ; y] -> ok @@ T_operator(TC_map, [x ; y])
    | "TC_big_map"  , [x ; y] -> ok @@ T_operator(TC_big_map, [x; y])
    | ("TC_contract" | "TC_option" | "TC_list" | "TC_set" | "TC_map" | "TC_big_map"), _ ->
      failwith "internal error: wrong number of arguments for type operator"

    | "TC_unit"      , [] -> ok @@ T_constant(TC_unit)
    | "TC_string"    , [] -> ok @@ T_constant(TC_string)
    | "TC_bytes"     , [] -> ok @@ T_constant(TC_bytes)
    | "TC_nat"       , [] -> ok @@ T_constant(TC_nat)
    | "TC_int"       , [] -> ok @@ T_constant(TC_int)
    | "TC_mutez"     , [] -> ok @@ T_constant(TC_mutez)
    | "TC_operation" , [] -> ok @@ T_constant(TC_operation)
    | "TC_address"   , [] -> ok @@ T_constant(TC_address)
    | "TC_key"       , [] -> ok @@ T_constant(TC_key)
    | "TC_key_hash"  , [] -> ok @@ T_constant(TC_key_hash)
    | "TC_chain_id"  , [] -> ok @@ T_constant(TC_chain_id)
    | "TC_signature" , [] -> ok @@ T_constant(TC_signature)
    | "TC_timestamp" , [] -> ok @@ T_constant(TC_timestamp)
    | _,               [] ->
      failwith "internal error: wrong number of arguments for type constant"
    | _                       ->
      failwith "internal error: unknown type operator"

  let string_of_type_operator = function
    | TC_contract                  , lst -> "TC_contract"                  , lst
    | TC_option                    , lst -> "TC_option"                    , lst
    | TC_list                      , lst -> "TC_list"                      , lst
    | TC_set                       , lst -> "TC_set"                       , lst
    | TC_map                       , lst -> "TC_map"                       , lst
    | TC_big_map                   , lst -> "TC_big_map"                   , lst
    | TC_map_or_big_map            , lst -> "TC_map_or_big_map"            , lst
    | TC_michelson_pair            , lst -> "TC_michelson_pair"            , lst
    | TC_michelson_or              , lst -> "TC_michelson_or"              , lst
    | TC_michelson_pair_right_comb , lst -> "TC_michelson_pair_right_comb" , lst
    | TC_michelson_pair_left_comb  , lst -> "TC_michelson_pair_left_comb"  , lst
    | TC_michelson_or_right_comb   , lst -> "TC_michelson_or_right_comb"   , lst
    | TC_michelson_or_left_comb    , lst -> "TC_michelson_or_left_comb"    , lst

  let string_of_type_constant = function
    | TC_unit      -> "TC_unit", []
    | TC_string    -> "TC_string", []
    | TC_bytes     -> "TC_bytes", []
    | TC_nat       -> "TC_nat", []
    | TC_int       -> "TC_int", []
    | TC_mutez     -> "TC_mutez", []
    | TC_operation -> "TC_operation", []
    | TC_address   -> "TC_address", []
    | TC_key       -> "TC_key", []
    | TC_key_hash  -> "TC_key_hash", []
    | TC_chain_id  -> "TC_chain_id", []
    | TC_signature -> "TC_signature", []
    | TC_timestamp -> "TC_timestamp", []

  let string_of_type_expression' = function
    | T_operator o -> string_of_type_operator o
    | T_constant c -> string_of_type_constant c
    | T_sum _ | T_record _ | T_arrow _ | T_variable _ ->
      failwith "not a type operator or constant"

end
