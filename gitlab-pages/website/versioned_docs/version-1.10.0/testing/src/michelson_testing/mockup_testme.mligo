(* This is mockup_testme.mligo *)
type storage = string

type result = operation list * storage

[@entry]
let append (s : string) (storage : storage) : result =
  [], storage ^ s

[@entry]
let reset (_ : unit) (_storage : storage) : result =
  [], ""