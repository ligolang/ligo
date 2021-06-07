module Errors = Errors
open Trace

let all_mapper = [
  Helpers.fold_to_map () Scoping.peephole
]

let all_module =
  let all_p = List.map ~f:Helpers.map_module all_mapper in
  bind_chain all_p

let all_expression =
  let all_p = List.map ~f:Helpers.map_expression all_mapper in
  bind_chain all_p

let fold_expression = Helpers.fold_expression

let map_expression  = Helpers.map_expression
