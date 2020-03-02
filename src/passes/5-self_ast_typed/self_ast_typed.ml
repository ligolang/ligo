open Trace

let all = []

let all_program =
  let all_p = List.map Helpers.map_program all in
  bind_chain all_p

let all_expression =
  let all_p = List.map Helpers.map_expression all in
  bind_chain all_p

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression
