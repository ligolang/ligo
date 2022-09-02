let all_expression_mapper = [
]

let all_program ~init =
  let all_p  = List.map ~f:Helpers.map_expression_in_declarations all_expression_mapper in
  List.fold ~init ~f:(|>) all_p

let all_expression ~raise p =
  let all_p = List.map ~f:(Helpers.map_expression ~raise) all_expression_mapper in
  List.fold ~f:(|>) all_p ~init:p

let fold_map_expression = Helpers.fold_map_expression
