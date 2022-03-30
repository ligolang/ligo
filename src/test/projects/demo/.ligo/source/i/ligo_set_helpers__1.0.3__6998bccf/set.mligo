
let union (xs : int set) (ys : int set) =
    let u = Set.fold (fun (s, x : (int set * int)) -> Set.add x s) xs (Set.empty : int set) in
    let u = Set.fold (fun (s, y : (int set * int)) -> Set.add y s) ys u in
    u

let inter (xs : int set) (ys : int set) = 
    if Set.cardinal xs > Set.cardinal ys
    then Set.fold (fun (s, y : (int set * int)) -> if Set.mem y xs then Set.add y s else s) ys (Set.empty : int set)
    else Set.fold (fun (s, x : (int set * int)) -> if Set.mem x ys then Set.add x s else s) xs (Set.empty : int set)

let to_list (xs : int set) = 
    Set.fold (fun (xs, x : (int list * int)) -> x::xs) xs ([] : int list)

let of_list (xs : int list) =
    List.fold (fun (s,x : (int set * int)) -> Set.add x s) xs (Set.empty : int set)