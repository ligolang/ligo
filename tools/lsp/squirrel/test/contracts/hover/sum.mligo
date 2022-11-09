type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return_ = operation list * storage

let main (action, store : parameter * storage) : return_ =
  (
    [],
    (match action with
      Increment n -> store + n
    | Decrement n -> store - n
    | Reset       -> 0)
  )
