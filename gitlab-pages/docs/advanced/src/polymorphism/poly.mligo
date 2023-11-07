let id (type a) (x : a) : a = x
let three_i : int = id 3
let three_s : string = id "three"
let rev (type a) (xs : a list) : a list =
  let rec rev (type a) ((xs, acc) : a list * a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev (xs, (x :: acc)) in
  rev (xs, ([] : a list))
let lint : int list = rev [1; 2; 3]
let lnat : nat list = rev [1n; 2n; 3n]