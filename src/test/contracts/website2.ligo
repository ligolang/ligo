// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is a + b

function subtract (const a : int ; const b : int) : int is a - b

// real entrypoint that re-routes the flow based on the action provided
function main (const p : action ; const s : int) : (list(operation) * int) is 
  ((nil : list(operation)),
    case p of
    | Increment (n) -> add (s, n)
    | Decrement (n) -> subtract (s, n)
    end)
