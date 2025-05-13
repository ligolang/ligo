let f (x : int) = x + 1
let g (x : int) = x - 2
let h (x : int) = x + x - 3
let result = h (g (f 42))
let result = 42 |> f |> g |> h
let result = f 42 |> g |> h