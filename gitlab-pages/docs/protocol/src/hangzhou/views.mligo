type storage = string
let main (((),s): unit * storage) : operation list * storage = [] , s

(* view 'view1', simply returns the storage *)
[@view] let view1 ((),s: unit * storage) : storage = s

(* view 'v2', returns true if the storage has a given length *)
[@view] let v2 (expected_length,s: nat * storage) : bool = (String.length s = expected_length)

(* view 'v3' returns a constant int *)
[@view] let v3 ((),_ : unit * storage) : int = 42
let view_call ((name,parameter,addr): string * int * address) : int option = Tezos.call_view "sto_plus_n" 1 addr