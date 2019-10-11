module Typer : sig
  open Trace
  open Ast_typed

  module Errors : sig
    val wrong_param_number : name -> int -> 'a list -> unit -> error
    val error_uncomparable_types : type_value -> type_value -> unit -> error
  end

  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * typer'

  (*
  val typer'_0 : name -> (type_value option -> type_value result) -> typer'
  *)
  val typer_0 : name -> ( type_value option -> type_value result ) -> typer
  (*
  val typer'_1 : name -> (type_value -> type_value result) -> typer'
  *)
  val typer_1 : name -> (type_value -> type_value result) -> typer
  (*
  val typer'_1_opt : name -> (type_value -> type_value option -> type_value result) -> typer'
  *)
  val typer_1_opt : name -> (type_value -> type_value option -> type_value result) -> typer
  (*
  val typer'_2 : name -> (type_value -> type_value -> type_value result) -> typer'
  *)
  val typer_2 : name -> (type_value -> type_value -> type_value result) -> typer
  (*
  val typer'_3 : name -> (type_value -> type_value -> type_value -> type_value result) -> typer'
  *)
  val typer_3 : name -> (type_value -> type_value -> type_value -> type_value result) -> typer
  (*
  val typer'_4 : name -> (type_value -> type_value -> type_value -> type_value -> type_value result) -> typer'
  *)
  val typer_4 : name -> (type_value -> type_value -> type_value -> type_value -> type_value result) -> typer
  (*
  val typer'_5 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer'
  *)
  val typer_5 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer
  (*
  val typer'_6 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer'
  *)
  val typer_6 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer

  val constant : name -> type_value -> typer 

  val eq_1 : type_value -> type_value -> bool
  val eq_2 : ( type_value * type_value ) -> type_value -> bool
  val assert_eq_1 : ?msg:string -> type_value -> type_value -> unit result

  val comparator : name -> typer
  val boolean_operator_2 : name -> typer

end

module Compiler : sig
  open Tezos_utils.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
    | Tetrary of michelson
    | Pentary of michelson
    | Hexary of michelson
  val simple_constant : t -> predicate
  val simple_unary : t -> predicate
  val simple_binary : t -> predicate
  val simple_ternary : t -> predicate
  val simple_tetrary : t -> predicate
  val simple_pentary : t -> predicate
  val simple_hexary : t -> predicate

end
