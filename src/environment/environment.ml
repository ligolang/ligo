open Stage_common
open Ast_typed
open Literal_types
module Protocols = Protocols


(* This is an env use by repl and build *)
(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t = module_
let pp ppf m = PP.module_ ppf @@ m
let add_module : ?public:unit -> ?hidden:unit -> Stage_common.ModuleVar.t -> Ast_typed.module_ -> t -> t = fun ?public ?hidden module_binder module_ env ->
  let module_ = Location.wrap @@ Declaration.M_struct module_ in
  let new_d = Location.wrap @@ Declaration.Declaration_module ({module_binder;module_=module_;module_attr={public=Option.is_some public;hidden=Option.is_some hidden}}) in
  let new_decl = Decl new_d in
  new_decl :: env

let add_declaration decl env = decl :: env
let append (program : program) env : t = List.fold_left ~f:(fun l m -> Decl m :: l ) ~init:env program

let fold ~f ~init (env:t) = List.fold ~f ~init @@ List.rev env

(* Artefact for build system *)
type core = Ast_core.module_
let add_core_module ?public ?hidden : ModuleVar.t -> Ast_core.module_ -> core -> core =
  fun module_binder module_ env ->
    let module_ = Location.wrap @@ Ast_core.Declaration.M_struct module_ in
    let new_d = Location.wrap @@ Ast_core.Declaration.Declaration_module {module_binder;module_;module_attr={public=Option.is_some public;hidden=Option.is_some hidden}} in
  let new_decl = Ast_core.Decl new_d in
    new_decl :: env

let to_module (env:t) : module_ = List.rev env

let to_program (env:t) : program =
  List.fold_left ~f:(fun l (Decl m) -> m :: l ) ~init:[] env

let append_core (program : S.program) env : core = List.fold_left ~f:(fun l m -> S.Decl m :: l ) ~init:env program
let init_core p = append_core p []
let to_core_module env = List.rev env
let to_core_program (env:core) : S.program =
  List.fold_left ~f:(fun l (Decl m) -> m :: l ) ~init:[] env

(* This is an stdlib *)
let star = Kind.Type
(*
  Make sure all the type value laoded in the environment have a `Ast_core` value attached to them (`type_meta` field of `type_expression`)
*)
let basic_types : (TypeVar.t * type_expression) list = [
    (v_bool   , t_bool                  ()) ;
    (v_string , t_string                ()) ;
    (v_bytes  , t_bytes                 ()) ;
    (v_int    , t_int                   ()) ;
    (v_nat    , t_nat                   ()) ;
    (v_unit   , t_unit                  ()) ;
    (v_option , t_option_abst           ()) ;
  ]

let michelson_base : (TypeVar.t * type_expression) list = [
    (v_operation          , t_operation                          ()) ;
    (v_tez                , t_constant     Tez                   []) ;
    (v_address            , t_address                            ()) ;
    (v_signature          , t_signature                          ()) ;
    (v_key                , t_key                                ()) ;
    (v_key_hash           , t_key_hash                           ()) ;
    (v_chest              , t_chest                              ()) ;
    (v_chest_key          , t_chest_key                          ()) ;
    (v_chest_opening_result , t_chest_opening_result             ()) ;
    (v_timestamp          , t_timestamp                          ()) ;
    (v_list               , t_abstraction1 List                star) ;
    (v_big_map            , t_abstraction2 Big_map        star star) ;
    (v_map                , t_abstraction2 Map            star star) ;
    (v_set                , t_abstraction1 Set                 star) ;
    (v_contract           , t_abstraction1 Contract            star) ;
    (v_michelson_or       , t_abstraction2 Michelson_or   star star) ;
    (v_michelson_pair     , t_abstraction2 Michelson_pair star star) ;
    (v_chain_id           , t_chain_id                           ()) ;
    (v_baker_hash         , t_baker_hash                         ()) ;
    (v_pvss_key           , t_pvss_key                           ()) ;
    (v_sapling_state      , t_abstraction1 Sapling_state       star) ;
    (v_sapling_trasaction , t_abstraction1 Sapling_transaction star) ;
    (v_baker_operation    , t_constant     Baker_operation       []) ;
    (v_bls12_381_g1       , t_bls12_381_g1                       ()) ;
    (v_bls12_381_g2       , t_bls12_381_g2                       ()) ;
    (v_bls12_381_fr       , t_bls12_381_fr                       ()) ;
    (v_never              , t_never                              ()) ;
    (v_ticket             , t_abstraction1 Ticket              star) ;
    (v_external_int       , t_abstraction1 (External "int")           star) ;
    (v_external_ediv      , t_abstraction2 (External "ediv")     star star) ;
    (v_external_u_ediv    , t_abstraction2 (External "u_ediv")     star star) ;
    (v_tx_rollup_l2_address, t_tx_rollup_l2_address ()             ) ;
]

let base = basic_types @ michelson_base
let jakarta_types = base

let meta_ligo_types : (TypeVar.t * type_expression) list -> (TypeVar.t * type_expression) list =
  fun proto_types ->
    proto_types @ [
    (v_test_michelson     , t_constant Michelson_program        []) ;
    (v_typed_address      , t_abstraction2 Typed_address star star) ;
    (v_mutation           , t_constant Mutation                 []) ;
    (v_michelson_contract , t_constant Michelson_contract       []) ;
    (v_gen                , t_abstraction1 Gen star               ) ;
  ]

let of_list_type : (TypeVar.t * type_expression) list -> t =
  List.map ~f:(fun (type_binder,type_expr) ->
    let d = Location.wrap @@ Ast_typed.Declaration.Declaration_type {type_binder;type_expr;type_attr={public=true;hidden=false}} in
    Ast_typed.Decl d)

let default : Protocols.t -> t = function
  | Protocols.Jakarta -> of_list_type jakarta_types
  | Protocols.Kathmandu -> of_list_type jakarta_types

let default_with_test : Protocols.t -> t = function
  | Protocols.Jakarta -> of_list_type (meta_ligo_types jakarta_types)
  | Protocols.Kathmandu -> of_list_type (meta_ligo_types jakarta_types)
