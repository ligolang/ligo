type storage = unit

let%entry main (p:unit) storage =
  (fun (f : int -> int -> int) (x : int) (y : int) -> (f y) (x + y))
  (fun (x : int) (y : int) -> x + y)
  0
  1
