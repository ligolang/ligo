(* This is mutation-contract.mligo *)
type storage = int

type result = operation list * storage

(* Two entrypoints *)
[@entry] let add (delta : int) (store : storage) : result = [], store + delta
[@entry] let sub (delta : int) (store : storage) : result = [], store - delta