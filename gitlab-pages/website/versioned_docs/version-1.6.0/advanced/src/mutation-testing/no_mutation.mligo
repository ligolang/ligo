(* This is mutation-contract.mligo *)
type storage = int

type result = operation list * storage

(* Two entrypoints *)
[@entry]
let add (delta : int) (store : storage) : result =
  [@no_mutation] let _ = assert (0 = 0) in
  [], store + delta

[@entry] [@no_mutation]
let sub (delta : int) (store : storage) : result =
  [], store - delta