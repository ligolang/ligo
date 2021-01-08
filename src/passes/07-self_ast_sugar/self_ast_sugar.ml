open Trace

let all_expression_mapper = [
]

let all_type_expression_mapper = [
]

let all_exp = List.map (fun el -> Helpers.Expression el) all_expression_mapper
let all_ty = List.map (fun el -> Helpers.Type_expression el) all_type_expression_mapper

let all_module =
  let all_p  = List.map Helpers.map_module all_exp in
  let all_p2 = List.map Helpers.map_module all_ty in
  bind_chain (List.append all_p all_p2)

let all_expression =
  let all_p = List.map Helpers.map_expression all_expression_mapper in
  bind_chain all_p

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression
