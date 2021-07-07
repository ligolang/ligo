open Ast_typed
open Stage_common.Constant
module Protocols = Protocols
let starvar = Var.of_name "will_be_ignored"
let star = t_variable @@ starvar (* This will later be replaced by the kind of the constant *)

let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_sum_ez [ ("true" ,t_unit ()); ("false",t_unit ()) ] ) ;
    (v_string , t_constant string_name []) ;
    (v_bytes , t_constant bytes_name []) ;
    (v_int , t_constant int_name []) ;
    (v_nat , t_constant nat_name []) ;
    (v_unit , t_constant unit_name []) ;
    (v_option , t_constant option_name [star]) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_constant operation_name []) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_constant address_name []) ;
    (v_signature , t_constant signature_name []) ;
    (v_key , t_constant key_name []) ;
    (v_key_hash , t_constant key_hash_name []) ;
    (v_timestamp , t_constant timestamp_name []) ;
    (v_list , t_constant list_name [star]) ;
    (v_big_map , t_constant big_map_name [star;star]);
    (v_map , t_constant map_name [star;star]) ;
    (v_set , t_constant set_name [star]);
    (v_contract , t_constant contract_name [star]);
    (v_map_or_big_map , t_constant map_or_big_map_name [star;star]);
    (v_michelson_or , t_constant michelson_or_name [star;star]);
    (v_michelson_pair , t_constant michelson_pair_name [star;star]);
    (v_chain_id , t_constant chain_id_name []) ;
    (v_michelson_pair_right_comb , t_constant michelson_pair_right_comb_name [star]);
    (v_michelson_pair_left_comb , t_constant michelson_pair_left_comb_name [star]);
    (v_michelson_or_right_comb , t_constant michelson_or_right_comb_name [star]);
    (v_michelson_or_left_comb , t_constant michelson_or_left_comb_name [star]);
    (v_baker_hash , t_constant baker_hash_name []);
    (v_pvss_key , t_constant pvss_key_name []);
    (v_sapling_state , t_constant sapling_state_name [star]);
    (v_sapling_trasaction , t_constant sapling_transaction_name [star]);
    (v_baker_operation , t_constant baker_operation_name []);
    (v_bls12_381_g1 , t_constant bls12_381_g1_name []);
    (v_bls12_381_g2 , t_constant bls12_381_g2_name []);
    (v_bls12_381_fr ,  t_constant bls12_381_fr_name []);
    (v_never , t_constant never_name []);
    (v_ticket , t_constant ticket_name [star]);
]

let edo_types = basic_types @ michelson_base

let meta_ligo_types : (type_variable * type_expression) list =
  edo_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_ligo , t_constant test_ligo_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_constant typed_address_name [star;star]) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]

let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type meta_ligo_types
