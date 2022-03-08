
let concat (xs : int list) (ys : int list) = 
    List.fold_right (fun (x,ys : (int * int list)) -> x::ys) xs ys

let reverse (xs : int list) = 
    List.fold_left (fun (ys,x : (int list * int)) -> x::ys) ([] : int list) xs