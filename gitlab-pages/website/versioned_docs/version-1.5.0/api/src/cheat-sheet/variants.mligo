type action =
  Increment of int
| Decrement of int
let a : action = Increment 5

let result : int =
  match a with
    Increment n -> n + 1
  | Decrement n -> n - 1