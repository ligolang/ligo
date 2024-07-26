type storage = string
type ret = operation list * storage

[@entry]
let main (word : string) (store : storage) : ret
  = [] , store ^ " " ^ word

(* view 'view1', simply returns the storage *)
[@view] let view1 (() : unit) (s : storage) : storage
  = s

(* view 'v2', returns true if the storage has a given length *)
[@view] let v2 (expected_length : nat) (s : storage) : bool
  = (String.length s = expected_length)

(* view 'v3' does not use its parameters and returns a constant int *)
[@view] let v3 (() : unit) (_ : storage) : int
  = 42