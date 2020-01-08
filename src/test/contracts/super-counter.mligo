type action =
| Increment of int
| Decrement of int

let main (ps : action * int) : (operation list * int) =
  let storage =
    match ps.0 with
    | Increment n -> ps.1 + n
    | Decrement n -> ps.1 - n in
  (([] : operation list) , storage)
