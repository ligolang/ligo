open Ast_typed
open Stage_common.Constant
module Protocols = Protocols


(* This is an env use by repl and build *)
(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t = module' 
let pp ppf m = PP.module' ppf @@ m
let add_module ?public module_binder module_ env =
  (Location.wrap @@ Declaration_module {module_binder;module_=module_;module_attr={public=Option.is_some public}}) :: env

let add_declaration decl env = decl :: env
let append program env = List.fold_left ~f:(fun l m -> m :: l ) ~init:env program

let fold ~f ~init (env:t) = List.fold ~f ~init @@ List.rev env
let init p = append p []

(* Artefact for build system *)
type core = Ast_core.module'
let add_core_module ?public : Ast_core.module_variable -> Ast_core.module' -> core -> core = fun module_binder module_ env ->
  (Location.wrap @@ Ast_core.Declaration_module {module_binder;module_=module_;module_attr={public=Option.is_some public}}) :: env

let to_program env = List.rev env
let init_core p = append p []
let to_core_program env = List.rev env
let append_core = append

(* This is an stdlib *)
let star = ()
(*
  Make sure all the type value laoded in the environment have a `Ast_core` value attached to them (`type_meta` field of `type_expression`)
*)
let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_bool ()) ;
    (v_string , t_string ()) ;
    (v_bytes , t_bytes ()) ;
    (v_int , t_int ()) ;
    (v_nat , t_nat ()) ;
    (v_unit , t_unit ()) ;
    (v_option , t_abstraction1 option_name star) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_operation ()) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_address ()) ;
    (v_signature , t_signature ()) ;
    (v_key , t_key ()) ;
    (v_key_hash , t_key_hash ()) ;
    (v_timestamp , t_timestamp ()) ;
    (v_list , t_abstraction1 list_name star) ;
    (v_big_map , t_abstraction2 big_map_name star star);
    (v_map , t_abstraction2 map_name star star) ;
    (v_set , t_abstraction1 set_name star);
    (v_contract , t_abstraction1 contract_name star);
    (v_map_or_big_map , t_abstraction2 map_or_big_map_name star star);
    (v_michelson_or , t_abstraction2 michelson_or_name star star);
    (v_michelson_pair , t_abstraction2 michelson_pair_name star star);
    (v_chain_id , t_chain_id ()) ;
    (v_baker_hash , t_baker_hash ()) ;
    (v_pvss_key , t_pvss_key ()) ;
    (v_sapling_state , t_abstraction1 sapling_state_name star) ;
    (v_sapling_trasaction , t_abstraction1 sapling_transaction_name star) ;
    (v_baker_operation , t_constant baker_operation_name []) ;
    (v_bls12_381_g1 , t_bls12_381_g1 ()) ;
    (v_bls12_381_g2 , t_bls12_381_g2 ()) ;
    (v_bls12_381_fr ,  t_bls12_381_fr ()) ;
    (v_never , t_never ()) ;
    (v_ticket , t_abstraction1 ticket_name star) ;
]

let hangzhou_extra : (type_variable * type_expression) list = [
  (v_chest , t_chest ());
  (v_chest_key , t_chest_key ());
  (v_chest_opening_result , t_chest_opening_result ());
]

let edo_types = basic_types @ michelson_base
let hangzhou_types = basic_types @ michelson_base @ hangzhou_extra

let meta_ligo_types : (type_variable * type_expression) list -> (type_variable * type_expression) list =
  fun proto_types ->
    proto_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_abstraction2 typed_address_name star star) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]

let of_list_type : (type_variable * type_expression) list -> t = List.map ~f:(fun (type_binder,type_expr) -> Location.wrap @@ Ast_typed.Declaration_type {type_binder;type_expr;type_attr={public=true}})

let default : Protocols.t -> t = function
  | Protocols.Edo -> of_list_type edo_types
  | Protocols.Hangzhou -> of_list_type hangzhou_types

let default_with_test : Protocols.t -> t = function
  | Protocols.Edo -> of_list_type (meta_ligo_types edo_types)
  | Protocols.Hangzhou -> of_list_type (meta_ligo_types hangzhou_types)
