let all_expression_mapper = [
]
let all_type_expression_mapper = [
]

let all_exp = List.map ~f:(fun el -> Helpers.Expression el) all_expression_mapper
let all_ty = List.map ~f:(fun el -> Helpers.Type_expression el) all_type_expression_mapper

let all_module ~raise p =
  let all_p  = List.map ~f:(Helpers.map_module ~raise) all_exp in
  let all_p2 = List.map ~f:(Helpers.map_module ~raise) all_ty in
  List.fold ~f:(|>) (List.append all_p all_p2) ~init:p

let all_expression ~raise p =
  let all_p = List.map ~f:(Helpers.map_expression ~raise) all_expression_mapper in
  List.fold ~f:(|>) all_p ~init:p

let fold_map_expression = Helpers.fold_map_expression