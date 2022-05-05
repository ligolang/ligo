module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let all_expression ~raise ~options e =
  let e = Helpers.map_expression (Polymorphic_replace.expression ~protocol:options.Compiler_options.middle_end.protocol_version) e in
  let e = if not options.Compiler_options.middle_end.test then Obj_ligo.check_obj_ligo ~raise e else e in
  let e = Monomorphisation.mono_polymorphic_expr e in
  let e = Uncurry.uncurry_expression ~raise e in
  e
