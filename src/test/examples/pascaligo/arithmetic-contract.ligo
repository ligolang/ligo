(*_*
  name: PascaLIGO Contract
  language: pascaligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment (1)
    storage: 0
  deploy:
    entrypoint: main
    storage: 0
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: (5, 6)
*_*)
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
  block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
  block { skip } with a - b

// real entrypoint that re-routes the flow based
// on the action provided
function main (const p : action ; const s : int) :
  (list(operation) * int) is
  block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
  end)
