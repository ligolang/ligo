type action is
| Increment of int
| Decrement of int

function increment(const i : int ; const n : int) : int is
  block { skip } with (i + n)

function decrement(const i : int ; const n : int) : int is
  block { skip } with (i - n)

function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : operation),
    case p of
    | Increment n -> increment(s , n)
    | Decrement n -> decrement(s , n)
    end)
