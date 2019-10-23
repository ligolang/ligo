type storage = unit

let%entry main (p:unit) storage =
  (fun (f : int -> int) (x : int) -> (f x))
  (fun (x : int) -> x)
  1
