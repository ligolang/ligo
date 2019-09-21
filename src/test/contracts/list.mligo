type storage = int * int list

type param = int list

let x : int list = []
let y : int list = [ 3 ; 4 ; 5 ]
let z : int list = 2 :: y

let%entry main (p : param) storage =
  let storage =
    match p with
      [] -> storage
    | hd::tl -> storage.(0) + hd, tl
  in (([] : operation list), storage)
