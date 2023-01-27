type 'a return = operation list * 'a

let main (() : unit) (s : int) : int return = ([]:operation list) , s

(* Tezos.create_contract can't be used in top-level of a view *)
let bad_view1 (n,s: int * int) : int =
  let _ = Tezos.create_contract main (None : key_hash option) 0mutez 2 in
  s + n + 1

(* Is ok if in a lambda. This example also shows that "forbidden"
   types like operation are allowed if they are under lambda *)
let ok_view ((),_: unit * int) : int -> operation * address =
  let f (s:int) = Tezos.create_contract main (None : key_hash option) 0mutez s in
  f

(* self forbidden anyway *)
let bad_view2 ((),_: unit * int) : unit contract =
  let x : unit contract = Tezos.self "%default" in
  x