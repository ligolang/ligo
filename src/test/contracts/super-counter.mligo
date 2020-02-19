type action =
| Increment of int
| Decrement of int

let test_param = Increment(1)
let test_storage = 2

let main (ps : action * int) : (operation list * int) =
  let storage =
    match ps.0 with
    | Increment n -> ps.1 + n
    | Decrement n -> ps.1 - n in
  (([] : operation list) , storage)
