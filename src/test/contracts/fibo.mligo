type storage = unit

let main (p: unit) storage =
  (fun (f: (int * int) -> int) (x: int) (y: int) -> f (y,x))
    (fun (x: int) (y: int) -> x + y)
    0
    1
