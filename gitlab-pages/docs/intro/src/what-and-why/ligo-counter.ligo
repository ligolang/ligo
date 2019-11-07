type action is
| Increment of int
| Decrement of int
| Reset of unit

function main (const p : action ; const s : int) : (list(operation) * int) is
 block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> s + n
  | Decrement(n) -> s - n
  | Reset(n) -> 0
 end)