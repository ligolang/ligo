let rec fib (n : int) : int =
  if n <= 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

let rec cat (type a) (xs : a list) (ys : a list) : a list =
  match xs with
  | [] -> ys
  | (x :: xs) -> x :: (cat xs ys)

let main ((p, s) : int list * int list) : operation list * int list =
  [], cat p s

let rec ackermann ((m, n) : nat * nat) : nat =
  if m = 0n then
    n + 1n
  else if n = 0n then
    ackermann (abs (m - 1), 1n)
  else
    ackermann (abs (m - 1), ackermann (m, abs (n - 1)))
