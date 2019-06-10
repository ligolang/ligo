type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type action =
| Increment of int
| Decrement of int

let add (a: int) (b: int) : int = a + b

let subtract (a: int) (b: int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let%entry main (p : action) storage =
 let storage =
   match p with
   | Increment n -> add storage n
   | Decrement n -> subtract storage n
 in (([] : operation list), storage)
