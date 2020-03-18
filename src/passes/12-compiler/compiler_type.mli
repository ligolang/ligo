open Trace
open Mini_c.Types
open Proto_alpha_utils.Memory_proto_alpha
open Protocol
open Script_ir_translator

module O = Tezos_utils.Michelson
(*
module Contract_types = Meta_michelson.Types
*)

module Ty : sig

  open Script_typed_ir
  (*
  open Script_int_repr
  *)
  (*
  val nat_k : n num comparable_ty
  val tez_k : Alpha_context.Tez.tez comparable_ty
  val int_k : z num comparable_ty
  val string_k : string comparable_ty
  val address_k : Alpha_context.Contract.t comparable_ty
  val timestamp_k : Alpha_context.Script_timestamp.t comparable_ty
  val bytes_k : Tezos_protocol_environment_alpha__Environment.MBytes.t comparable_ty
  (* val timestamp_k = Timestamp_key None *)
*)
(*
  val unit : unit ty
  val bytes : Tezos_protocol_environment_alpha__Environment.MBytes.t ty
  val nat : n num ty
  val tez : Alpha_context.Tez.tez ty
  val int : z num ty
  *)
  val big_map : 'a comparable_ty -> 'b ty -> ( 'a , 'b ) big_map ty
  val signature : Alpha_context.signature ty
  (*
  val operation : Alpha_context.packed_internal_operation ty
  val bool : bool ty
  *)
  val mutez : Alpha_context.Tez.tez ty
  (*
  val string : string ty
  *)
  val key : Alpha_context.public_key ty
  (*
  val list : 'a ty -> 'a list ty
  val set : 'a comparable_ty -> 'a set ty
  val address : Alpha_context.Contract.t ty
  val option : 'a ty -> 'a option ty
  val contract : 'a ty -> 'a typed_contract ty
  val lambda : 'a ty -> 'b ty -> ( 'a , 'b ) lambda ty
  val timestamp : Alpha_context.Script_timestamp.t ty
  val map : 'a comparable_ty -> 'b ty -> ( 'a , 'b ) map ty
  val pair : 'a ty -> 'b ty -> ('a , 'b ) pair ty
  *)
  val union : 'a ty -> 'b ty -> ( 'a , 'b ) union ty
  (*
  val not_comparable : string -> unit -> error
  val not_compilable_type : string -> unit -> error

  val comparable_type_base : type_base -> ex_comparable_ty result
  val comparable_type : type_value -> ex_comparable_ty result
  val base_type : type_base -> ex_ty result
 *)
  val type_ : type_value -> ex_ty result

  val environment_representation : environment -> ex_ty result

  val environment : environment -> ex_stack_ty result
  (*
  val not_comparable : string -> unit -> error
  val not_compilable_type : string -> unit -> error

  val comparable_type_base : type_base -> ex_comparable_ty result

  val comparable_type : type_value -> ex_comparable_ty result

  val base_type : type_base -> ex_ty result

  *)
end

val type_ : type_value -> O.t result

val environment_element : string * type_value -> (int, O.prim) Tezos_micheline.Micheline.node result

val environment : ( 'a * type_value ) list -> O.t list result
val lambda_closure : environment * type_value  * type_value -> (int, O.prim) Tezos_micheline.Micheline.node result

val environment_closure : environment -> (int , O.prim ) Tezos_micheline.Micheline.node result
(*
val base_type : type_base -> O.michelson result

*)
