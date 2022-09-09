type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage
