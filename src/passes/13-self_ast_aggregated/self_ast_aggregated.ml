module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise = Obj_ligo.check_obj_ligo ~raise
let expression_mono = Monomorphisation.mono_polymorphic_expr
let expression_uncurry ~raise = Uncurry.uncurry_expression ~raise

let all_expression ~raise e =
  let e = expression_obj ~raise e in
  let e = Monomorphisation.mono_polymorphic_expr e in
  let e = Uncurry.uncurry_expression ~raise e in
  e
