open Ast_typed.Types
include Plugin
val get_constructors_by_lhs : 'type_variable -> 'type_variable t -> c_constructor_simpl MultiSet.t
val get_rows_by_lhs : 'type_variable -> 'type_variable t -> c_row_simpl MultiSet.t
val get_polys_by_lhs : 'type_variable -> 'type_variable t -> c_poly_simpl MultiSet.t

type 'type_variable t_for_tests = {
  constructor : ('type_variable * c_constructor_simpl MultiSet.t) list ;
  poly        : ('type_variable * c_poly_simpl MultiSet.t) list ;
  row         : ('type_variable * c_row_simpl MultiSet.t) list ;
}
val bindings : 'type_variable t -> 'type_variable t_for_tests
