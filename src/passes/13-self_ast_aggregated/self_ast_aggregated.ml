module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let all_expression ~raise ~(options : Compiler_options.middle_end) e =
  let e = Helpers.map_expression (Polymorphic_replace.expression ~protocol:options.protocol_version) e in
  let e = if not options.test then Obj_ligo.check_obj_ligo ~raise e else e in
  let e = Purify_assignations.expression e in
  let e = Monomorphisation.mono_polymorphic_expr e in
  let e = Uncurry.uncurry_expression e in
  e
