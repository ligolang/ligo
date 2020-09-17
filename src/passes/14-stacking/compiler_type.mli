open Errors
open Trace
open Co_de_bruijn

module O = Tezos_utils.Michelson
(*
module Contract_types = Meta_michelson.Types
*)

val type_ : type_expression -> (O.t, stacking_error) result

val environment_element : string * type_expression -> (O.t, stacking_error) result

val environment : ( 'a * type_expression ) list -> (O.t list , stacking_error) result
val lambda_closure_with_ty : environment * type_expression  * type_expression ->
                     (O.michelson * O.michelson * O.michelson, stacking_error) result

val lambda_closure : environment * type_expression  * type_expression -> (O.t, stacking_error) result

val environment_closure : environment -> (O.t, stacking_error) result
