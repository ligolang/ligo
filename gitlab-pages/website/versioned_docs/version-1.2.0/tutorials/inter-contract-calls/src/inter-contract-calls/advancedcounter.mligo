(* examples/contracts/mligo/AdvancedCounter.mligo *)

let nop : operation list = []

[@entry] let set      (n : int)  (_storage : int) = nop, n
[@entry] let add      (n : int)  (storage : int)  = nop, storage + n
[@entry] let subtract (n : int)  (storage : int)  = nop, storage + n
[@entry] let multiply (n : int)  (storage : int)  = nop, storage * n
[@entry] let reset    (_ : unit) (_storage : int) = nop, 0