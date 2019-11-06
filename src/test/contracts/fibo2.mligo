type storage = unit

let main (p: unit) storage =
  (fun (f: int -> int) (_: int) (y: int) -> f y)
    (fun (x: int) -> x)
    0
    1
