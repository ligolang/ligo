type storage = unit

let main (p : unit) (s : storage) =
  (fun (f : int -> int) (x : int) -> f x)
    (fun (x : int) -> x)
    1
