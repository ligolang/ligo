
let f  (x : int)           = x + 1
let gg (x : int) (y : int) = x + y - 1
let h  (x : int) : nat     = abs x

let typing_error = f 42 |> gg |> h
