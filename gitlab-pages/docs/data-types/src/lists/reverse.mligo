let rev (type a) (xs : a list) : a list =
  let rec rev (type a) (xs : a list) (acc : a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev xs (x::acc)
  in rev xs []
let ints : int list = rev [1; 2; 3]
let nats : nat list = rev [1n; 2n; 3n]