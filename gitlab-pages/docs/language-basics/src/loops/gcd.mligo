let rec iter (x, y : nat * nat) : nat =
  if y = 0n then x else iter (y, x mod y)

let gcd (x, y : nat * nat) : nat =
  let x, y = if x < y then y,x else x,y
  in iter (x, y)