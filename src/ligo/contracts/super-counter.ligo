type action is
| Increment of int
| Decrement of int

function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : operation),
    case p of
    | Increment n -> s + n
    | Decrement n -> s - n
    end)
