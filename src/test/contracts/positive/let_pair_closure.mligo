let main (p : nat * string) (_ : unit -> unit) : operation list * (unit -> unit) =
  let (x, y) = p in
  let f = fun (_ : unit) : unit -> failwith (x + x + String.length y) in
  (([] : operation list), f)
