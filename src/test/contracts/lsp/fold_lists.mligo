let x = [1; 2; 3; 4; 5]
let y = [1; 2; 3]
module A = struct
    let bar = [4; 4; 4]
    let baz = [7; 8; 9]
end
let rec concat (l1 : int list) (l2 : int list) : int list =
    match l1 with
    | [] -> l2
    | x :: xs -> x :: (concat xs l2)
let foo x =
    let a = [[]; [1; 2; 3]; [1; 2; 3; 4; 5]] in
    List.fold_left (fun (acc, l) -> concat acc (List.map (fun y -> x + y) l)) [] a
