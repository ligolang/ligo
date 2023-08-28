type 'a return = operation list * 'a

[@entry]
let main (() : unit) (s : int) : int return = ([] : operation list), s

(* Is ok if in a lambda. This example also shows that "forbidden"
   types like operation are allowed if they are under lambda *)

[@view]
let ok_view (() : unit) (_ : int) : int -> operation * address =
  let f (s : int) = Tezos.create_contract main (None : key_hash option) 0mutez s in
  f
