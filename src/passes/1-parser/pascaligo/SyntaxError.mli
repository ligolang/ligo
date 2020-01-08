type t =
  Reserved_name       of AST.variable
| Duplicate_parameter of AST.variable
| Duplicate_variant   of AST.variable
| Non_linear_pattern  of AST.variable

type error = t

exception Error of t

module Ord :
  sig
    type t = AST.variable
    val compare : t -> t -> int
  end

module VarSet : Set.S with type elt = Ord.t

val check_reserved_name     : AST.variable -> AST.variable
val check_reserved_name_opt : AST.variable option -> unit
val check_reserved_names    : VarSet.t -> VarSet.t
val check_pattern           : AST.pattern -> unit
val check_variants          : AST.variant Region.reg list -> unit
val check_parameters        : AST.param_decl list -> unit
