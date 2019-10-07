
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
