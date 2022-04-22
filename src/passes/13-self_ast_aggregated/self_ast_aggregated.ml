module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let all_expression ~raise ?(test = false) e =
  let e = if not test then Obj_ligo.check_obj_ligo ~raise e else e in
  let e = Monomorphisation.mono_polymorphic_expr e in
  let e = Uncurry.uncurry_expression ~raise e in
  let e = Create_contract.create_contract ~raise e in
  e
