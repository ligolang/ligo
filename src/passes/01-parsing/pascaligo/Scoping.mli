(* This module exports checks on scoping, called from the parser. *)

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Pascaligo

(* Errors *)

type t =
  Reserved_name       of CST.variable
| Duplicate_parameter of CST.variable
| Duplicate_variant   of CST.variable
| Non_linear_pattern  of CST.variable
| Duplicate_field     of CST.variable

type error = t

exception Error of t

val check_reserved_name : CST.variable -> unit
val check_pattern       : CST.pattern -> unit
val check_variants      : CST.variant Region.reg list -> unit
val check_parameters    : CST.param_decl list -> unit
val check_fields        : CST.field_decl Region.reg list -> unit
