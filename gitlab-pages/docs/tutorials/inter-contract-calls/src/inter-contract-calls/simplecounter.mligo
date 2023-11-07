(* examples/contracts/mligo/SimpleCounter.mligo *)

[@entry]
let main (param : int) (storage : int) : operation list * int = [], param + storage