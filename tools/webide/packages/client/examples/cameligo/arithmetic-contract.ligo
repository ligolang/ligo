(*_*
  name: Cameligo Contract
  language: cameligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment 1
    storage: 0
  deploy:
    entrypoint: main
    storage: 0
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: 5, 6
*_*)
type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type action =
| Increment of int
| Decrement of int

let add (a,b: int * int) : int = a + b
let sub (a,b: int * int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let main (p,s: action * storage) =
 let storage =
   match p with
   | Increment n -> add (s, n)
   | Decrement n -> sub (s, n)
 in ([] : operation list), storage
