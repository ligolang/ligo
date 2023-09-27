(* This is testnew.mligo *)

type storage = int
type result = operation list * storage

[@entry] let increment (delta : int) (store : storage) : result = [], store + delta
[@entry] let decrement (delta : int) (store : storage) : result = [], store - delta
[@entry] let reset (_ : unit) (_store : storage) : result = [], 0