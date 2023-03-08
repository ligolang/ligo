open Ligo_prim
open Ast_typed
open Literal_types
module Protocols = Protocols

(* This is an env use by repl and build *)
(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t = program

let pp ppf m = PP.module_ ppf @@ m
let loc = Location.env

let add_module
    :  ?public:unit -> ?hidden:unit -> Ligo_prim.Module_var.t -> Ast_typed.module_ -> t
    -> t
  =
 fun ?public ?hidden module_binder module_ env ->
  let module_ = Location.wrap ~loc @@ Module_expr.M_struct module_ in
  let new_d =
    Location.wrap ~loc
    @@ D_module
         { module_binder
         ; module_
         ; module_attr =
             { public = Option.is_some public; hidden = Option.is_some hidden }
         }
  in
  new_d :: env


let add_declaration decl env = decl :: env
let append env (program : program) : t = List.rev_append program env
let fold ~f ~init (env : t) = List.fold ~f ~init (List.rev env)
let foldi ~f ~init (env : t) = List.foldi ~f ~init (List.rev env)

(* Artefact for build system *)
type core = Ast_core.module_

let add_core_module ?public ?hidden : Module_var.t -> Ast_core.module_ -> core -> core =
 fun module_binder module_ env ->
  let module_ = Location.wrap ~loc @@ Module_expr.M_struct module_ in
  let new_d =
    Location.wrap ~loc
    @@ Ast_core.D_module
         { module_binder
         ; module_
         ; module_attr =
             { public = Option.is_some public; hidden = Option.is_some hidden }
         }
  in
  new_d :: env


let to_module (env : t) : module_ = List.rev env
let to_program (env : t) : program = List.rev env

let append_core (env : core) (program : Ast_core.program) : core =
  List.rev_append program env


let init_core program = List.rev program
let to_core_module env = List.rev env
let to_core_program (env : core) : S.program = List.rev env

(* This is an stdlib *)
let star = Kind.Type

(*
  Make sure all the type value laoded in the environment have a `Ast_core` value attached to them (`type_meta` field of `type_expression`)
*)
let basic_types : (Type_var.t * type_expression) list =
  [ v_string ~loc, t_string ~loc ()
  ; v_bytes ~loc, t_bytes ~loc ()
  ; v_int ~loc, t_int ~loc ()
  ; v_nat ~loc, t_nat ~loc ()
  ; v_unit ~loc, t_unit ~loc ()
  ]


let michelson_base : (Type_var.t * type_expression) list =
  [ v_operation ~loc, t_operation ~loc ()
  ; v_tez ~loc, t_constant ~loc Tez []
  ; v_address ~loc, t_address ~loc ()
  ; v_signature ~loc, t_signature ~loc ()
  ; v_key ~loc, t_key ~loc ()
  ; v_key_hash ~loc, t_key_hash ~loc ()
  ; v_chest ~loc, t_chest ~loc ()
  ; v_chest_key ~loc, t_chest_key ~loc ()
  ; v_chest_opening_result ~loc, t_chest_opening_result ~loc ()
  ; v_timestamp ~loc, t_timestamp ~loc ()
  ; v_list ~loc, t_abstraction1 ~loc List star
  ; v_big_map ~loc, t_abstraction2 ~loc Big_map star star
  ; v_map ~loc, t_abstraction2 ~loc Map star star
  ; v_set ~loc, t_abstraction1 ~loc Set star
  ; v_contract ~loc, t_abstraction1 ~loc Contract star
  ; v_michelson_or ~loc, t_abstraction2 ~loc Michelson_or star star
  ; v_michelson_pair ~loc, t_abstraction2 ~loc Michelson_pair star star
  ; v_chain_id ~loc, t_chain_id ~loc ()
  ; v_baker_hash ~loc, t_baker_hash ~loc ()
  ; v_pvss_key ~loc, t_pvss_key ~loc ()
  ; v_sapling_state ~loc, t_abstraction1 ~loc Sapling_state star
  ; v_sapling_trasaction ~loc, t_abstraction1 ~loc Sapling_transaction star
  ; v_baker_operation ~loc, t_constant ~loc Baker_operation []
  ; v_bls12_381_g1 ~loc, t_bls12_381_g1 ~loc ()
  ; v_bls12_381_g2 ~loc, t_bls12_381_g2 ~loc ()
  ; v_bls12_381_fr ~loc, t_bls12_381_fr ~loc ()
  ; v_never ~loc, t_never ~loc ()
  ; v_ticket ~loc, t_abstraction1 ~loc Ticket star
  ; v_external_int ~loc, t_abstraction1 ~loc (External "int") star
  ; v_external_ediv ~loc, t_abstraction2 ~loc (External "ediv") star star
  ; v_external_u_ediv ~loc, t_abstraction2 ~loc (External "u_ediv") star star
  ; v_external_and ~loc, t_abstraction2 ~loc (External "and") star star
  ; v_external_u_and ~loc, t_abstraction2 ~loc (External "u_and") star star
  ; v_tx_rollup_l2_address ~loc, t_tx_rollup_l2_address ~loc ()
  ]


let base = basic_types @ michelson_base
let kathmandu_types = base
let lima_types = base

let meta_ligo_types
    : (Type_var.t * type_expression) list -> (Type_var.t * type_expression) list
  =
 fun proto_types ->
  proto_types
  @ [ v_test_michelson ~loc, t_constant ~loc Michelson_program []
    ; v_typed_address ~loc, t_abstraction2 ~loc Typed_address star star
    ; v_mutation ~loc, t_constant ~loc Mutation []
    ; v_michelson_contract ~loc, t_constant ~loc Michelson_contract []
    ; v_ast_contract ~loc, t_constant ~loc Ast_contract []
    ; v_gen ~loc, t_abstraction1 ~loc Gen star
    ; v_int64 ~loc, t_constant ~loc Int64 []
    ; v_views ~loc, t_abstraction1 ~loc Views star
    ]


let of_list_type : (Type_var.t * type_expression) list -> t =
  List.map ~f:(fun (type_binder, type_expr) ->
      Location.wrap ~loc
      @@ D_type { type_binder; type_expr; type_attr = { public = true; hidden = false } })


let default : Protocols.t -> t = function
  | Protocols.Lima -> of_list_type (meta_ligo_types lima_types)
  | Protocols.Kathmandu -> of_list_type (meta_ligo_types kathmandu_types)
