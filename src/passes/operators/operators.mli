
module Simplify : sig

  module Pascaligo : sig
    val constants : ( string * string ) list
    val type_constants : ( string * string ) list
  end

  module Camligo : sig
    val constants : ( string * string ) list
    val type_constants : ( string * string ) list
  end

  module Ligodity : sig
    val constants : ( string * string ) list
    val type_constants : ( string * string ) list
  end

end

module Typer : sig
  open Helpers.Typer
  open Ast_typed

  module Operators_types : sig
    (* TODO: we need a map from type names to type values. Then, all
       these bindings don't need to be exported anymore. *)
      val tc_subarg :
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_sizearg :
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_packable :
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_timargs :
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_divargs :
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_modargs :
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val tc_addargs :
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value ->
        Typesystem.Core.type_value -> Typesystem.Core.type_constraint
      val t_none : Typesystem.Core.type_value
      val t_sub : Typesystem.Core.type_value
      val t_some : Typesystem.Core.type_value
      val t_map_remove : Typesystem.Core.type_value
      val t_map_add : Typesystem.Core.type_value
      val t_map_update : Typesystem.Core.type_value
      val t_map_mem : Typesystem.Core.type_value
      val t_map_find : Typesystem.Core.type_value
      val t_map_find_opt : Typesystem.Core.type_value
      val t_map_fold : Typesystem.Core.type_value
      val t_map_map : Typesystem.Core.type_value
      val t_map_map_fold : Typesystem.Core.type_value
      val t_map_iter : Typesystem.Core.type_value
      val t_size : Typesystem.Core.type_value
      val t_slice : Typesystem.Core.type_value
      val t_failwith : Typesystem.Core.type_value
      val t_get_force : Typesystem.Core.type_value
      val t_int : Typesystem.Core.type_value
      val t_bytes_pack : Typesystem.Core.type_value
      val t_bytes_unpack : Typesystem.Core.type_value
      val t_hash256 : Typesystem.Core.type_value
      val t_hash512 : Typesystem.Core.type_value
      val t_blake2b : Typesystem.Core.type_value
      val t_hash_key : Typesystem.Core.type_value
      val t_check_signature : Typesystem.Core.type_value
      val t_sender : Typesystem.Core.type_value
      val t_source : Typesystem.Core.type_value
      val t_unit : Typesystem.Core.type_value
      val t_amount : Typesystem.Core.type_value
      val t_address : Typesystem.Core.type_value
      val t_now : Typesystem.Core.type_value
      val t_transaction : Typesystem.Core.type_value
      val t_get_contract : Typesystem.Core.type_value
      val t_abs : Typesystem.Core.type_value
      val t_cons : Typesystem.Core.type_value
      val t_assertion : Typesystem.Core.type_value
      val t_times : Typesystem.Core.type_value
      val t_div : Typesystem.Core.type_value
      val t_mod : Typesystem.Core.type_value
      val t_add : Typesystem.Core.type_value
      val t_set_mem : Typesystem.Core.type_value
      val t_set_add : Typesystem.Core.type_value
      val t_set_remove : Typesystem.Core.type_value
      val t_not : Typesystem.Core.type_value
    end

  (*
  val none : typer
  val set_empty : typer
  val sub : typer
  val some : typer
  val map_remove : typer 
  val map_add : typer 
  val map_update : typer 
  val map_mem : typer 
  val map_find : typer 
  *)
  val map_find_opt : typer 
  (*
  val map_iter : typer 
  val map_map : typer 
  val map_fold : typer 
  val big_map_remove : typer 
  val big_map_add : typer 
  val big_map_update : typer 
  val big_map_mem : typer 
  val big_map_find : typer 
  val size : typer
  val slice : typer
  val failwith_ : typer
  val get_force : typer
  val int : typer 
  val bytes_pack : typer
  val bytes_unpack : typer
  val hash256 : typer
  val hash512 : typer
  val blake2b : typer
  val hash_key : typer
  val check_signature : typer
  val sender : typer
  val source : typer
  val unit : typer
  val amount : typer
  *)
  val balance : typer
  (*
  val address : typer
  val now : typer
  val transaction : typer
  *)
  val originate : typer
  (*
  val get_contract : typer
  *)
  val set_delegate : typer
  (*
  val abs : typer
  val neg : typer
  val assertion : typer
  val times : typer
  val div : typer
  val mod_ : typer
  val add : typer
  val set_mem : typer
  val set_add : typer
  val set_remove : typer
  val set_iter : typer
  val list_iter : typer
  val list_map : typer
  val not_ : typer
  val or_ : typer
  val xor : typer
  val and_ : typer
  *)
  val lsl_ : typer
  val lsr_ : typer
  (*
  val concat : typer
  *)
  val cons : typer
  val constant_typers : typer' type_name_map

end

module Compiler : sig
  (*
  include Helpers.Compiler
  *)
  open Tezos_utils.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
    | Tetrary of michelson
    | Pentary of michelson
    | Hexary of michelson
  val operators : predicate Map.String.t
  val simple_constant : t -> predicate
  val simple_unary : t -> predicate
  val simple_binary : t -> predicate
  val simple_ternary : t -> predicate
  val simple_tetrary : t -> predicate
  val simple_pentary : t -> predicate
  val simple_hexary : t -> predicate

(*
  val predicates : predicate Map.String.t
*)
end
