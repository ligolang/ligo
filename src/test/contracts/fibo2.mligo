type storage = unit

let main (p : unit; store : storage) : operation list * storage =
  let n =
    (fun (f : int -> int) (z : int) (y : int) -> f y)
      (fun (x : int) -> x)
      0
      1
  in ([] : operation list), store
