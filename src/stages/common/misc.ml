open Types
open Trace

let map_type_operator f = function
    TC_contract x -> TC_contract (f x)
  | TC_option x -> TC_option (f x)
  | TC_list x -> TC_list (f x)
  | TC_set x -> TC_set (f x)
  | TC_map (x , y) -> TC_map (f x , f y)
  | TC_big_map (x , y)-> TC_big_map (f x , f y)

let bind_map_type_operator f = function
    TC_contract x -> let%bind x = f x in ok @@ TC_contract x
  | TC_option x -> let%bind x = f x in ok @@ TC_option x
  | TC_list x -> let%bind x = f x in ok @@ TC_list x
  | TC_set x -> let%bind x = f x in ok @@ TC_set x
  | TC_map (x , y) -> let%bind x = f x in let%bind y = f y in ok @@ TC_map (x , y)
  | TC_big_map (x , y)-> let%bind x = f x in let%bind y = f y in ok @@ TC_big_map (x , y)

let type_operator_name = function
      TC_contract _ -> "TC_contract"
    | TC_option   _ -> "TC_option"
    | TC_list     _ -> "TC_list"
    | TC_set      _ -> "TC_set"
    | TC_map      _ -> "TC_map"
    | TC_big_map  _ -> "TC_big_map"

let type_expression'_of_string = function
  | "TC_contract" , [x]     -> ok @@ T_operator(TC_contract x)
  | "TC_option"   , [x]     -> ok @@ T_operator(TC_option x)
  | "TC_list"     , [x]     -> ok @@ T_operator(TC_list x)
  | "TC_set"      , [x]     -> ok @@ T_operator(TC_set x)
  | "TC_map"      , [x ; y] -> ok @@ T_operator(TC_map (x , y))
  | "TC_big_map"  , [x ; y] -> ok @@ T_operator(TC_big_map (x, y))
  | ("TC_contract" | "TC_option" | "TC_list" | "TC_set" | "TC_map" | "TC_big_map"), _ ->
     failwith "internal error: wrong number of arguments for type operator"

  | "TC_unit"      , [] -> ok @@ T_constant(TC_unit)
  | "TC_string"    , [] -> ok @@ T_constant(TC_string)
  | "TC_bytes"     , [] -> ok @@ T_constant(TC_bytes)
  | "TC_nat"       , [] -> ok @@ T_constant(TC_nat)
  | "TC_int"       , [] -> ok @@ T_constant(TC_int)
  | "TC_mutez"     , [] -> ok @@ T_constant(TC_mutez)
  | "TC_bool"      , [] -> ok @@ T_constant(TC_bool)
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
  | TC_contract  x       -> "TC_contract" , [x]
  | TC_option    x       -> "TC_option"   , [x]
  | TC_list      x       -> "TC_list"     , [x]
  | TC_set       x       -> "TC_set"      , [x]
  | TC_map       (x , y) -> "TC_map"      , [x ; y]
  | TC_big_map   (x , y) -> "TC_big_map"  , [x ; y]

let string_of_type_constant = function
  | TC_unit      -> "TC_unit", []
  | TC_string    -> "TC_string", []
  | TC_bytes     -> "TC_bytes", []
  | TC_nat       -> "TC_nat", []
  | TC_int       -> "TC_int", []
  | TC_mutez     -> "TC_mutez", []
  | TC_bool      -> "TC_bool", []
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
  | T_tuple _|T_sum _|T_record _|T_arrow (_, _)|T_variable _ ->
     failwith "not a type operator or constant"
