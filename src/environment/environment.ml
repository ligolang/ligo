open Ast_typed

let merge e1 e2 = 
  let e1 = List.Ne.to_list e1 in
  let e2 = List.Ne.to_list e2 in
  List.Ne.of_list @@ e1 @ e2
 

let default = Environment.full_empty

let default = merge default Bool.environment
