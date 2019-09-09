type action =
| Increment of int
| Decrement of int

let main (p : action) (s : int) : (operation list * int) =
  let storage =
    match p with
    | Increment n -> s + n
    | Decrement n -> s - n in
  (([] : operation list) , storage)
