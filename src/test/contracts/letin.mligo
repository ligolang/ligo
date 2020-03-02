type storage = int * int

let main (n : int * storage) : operation list * storage =
  let x : int * int =
    let x : int = 7
    in x + n.0, n.1.0 + n.1.1
  in ([] : operation list), x
