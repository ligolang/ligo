type storage = unit

let%entry main (p:unit) storage =
  (fun (f : int -> int) (x : int) (y : int) -> (f y))
  (fun (x : int) -> x)
  0
  1
