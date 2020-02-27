type storage = unit

let main (p : unit; store : storage) : operation list * storage =
  let n =
    (fun (f : int -> int -> int) (x : int) (y : int) -> f y (x+y))
      (fun (x : int) (y : int) -> x + y)
      0
      1
  in ([] : operation list), store
