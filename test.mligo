let rec fibo2 ((n,n_1,n_0):int*int*int) : int =
  let fibo2 : int -> int = fun (k : int) -> k in
  if (n < 2) then n_1 else fibo2 3

let main (p,s : unit * int) : operation list * int =
  let x : int = fibo2 (5, 1, 1) in
  (([] : operation list), x)

