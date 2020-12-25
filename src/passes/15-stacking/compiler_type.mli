open Errors
open Trace
open Mini_c

module O = Tezos_utils.Michelson
(*
module Contract_types = Meta_michelson.Types
*)

val type_ : type_expression -> (unit O.t, stacking_error) result

val environment_element : string * type_expression -> (unit O.t, stacking_error) result

val environment : ( 'a * type_expression ) list -> (unit O.t list , stacking_error) result

val lambda_closure : type_expression list * type_expression  * type_expression -> (unit O.t, stacking_error) result

val environment_closure : type_expression list -> (unit O.t, stacking_error) result
