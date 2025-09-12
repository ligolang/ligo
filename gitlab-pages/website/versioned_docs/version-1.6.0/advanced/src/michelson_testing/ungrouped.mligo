(* This is mockup_testme.mligo *)
type storage = string

type result = operation list * storage

[@entry]
let append (s : string) (store : storage) : result =
 [],    (* No operations *)
 store ^ s