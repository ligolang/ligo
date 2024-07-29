type storage = string
type ret = operation list * storage

[@entry]
let main (word : string) (storage : storage) : ret
  = [] , storage ^ " " ^ word

(* This view returns the storage *)
[@view] let view1 (() : unit) (storage : storage) : storage
  = storage

(* This view returns true if the storage has a given length *)
[@view] let view2 (expected_length : nat) (storage : storage) : bool
  = (String.length storage = expected_length)

(* This view does not use the parameters or storage and returns a constant int *)
[@view] let view3 (() : unit) (_ : storage) : int
  = 42