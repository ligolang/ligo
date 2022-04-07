
let concat (xs : int list) (ys : int list) = 
    List.fold_right (fun (x,ys : (int * int list)) -> x::ys) xs ys

let reverse (xs : int list) = 
    List.fold_left (fun (ys,x : (int list * int)) -> x::ys) ([] : int list) xs

let sum (xs : int list) = 
    List.fold (fun (x,acc : (int * int)) -> acc + x) (xs : int list) 0