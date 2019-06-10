type storage = unit

(* not supported yet
let%entry main (p:unit) storage =
  (fun x -> ()) ()
*)

let%entry main (p:unit) storage =
  (fun (f : int -> int -> int) (x : int) (y : int) -> f y (x + y))
  (fun (x : int) (y : int) -> x + y)
  0
  1
