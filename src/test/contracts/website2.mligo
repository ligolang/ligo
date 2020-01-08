type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type action =
| Increment of int
| Decrement of int

let add (a: int) (b: int) : int = a + b
let sub (a: int) (b: int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let main (ps: action * storage) =
 let storage =
   match ps.0 with
   | Increment n -> add ps.1 n
   | Decrement n -> sub ps.1 n
 in ([] : operation list), storage
