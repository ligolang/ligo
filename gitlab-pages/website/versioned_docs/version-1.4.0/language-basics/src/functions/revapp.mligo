let f (x : int) = x + 1
let g (x : int) = x - 2
let h (x : int) = x + x - 3

(* Here we apply function f on value 42,
   then apply g on the result,
   and then apply h on the result *)
let result = h (g (f 42))

(* Parentheses are indeed necessary here. If we remove them, we have : *)
// let result' = h g f 42
(* Which is different, it is equivalent to : *)
// let result' = ((h g) f) 42
let result = 42 |> f |> g |> h
let result = f 42 |> g |> h