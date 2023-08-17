type 'a return = operation list * 'a

let main (() : unit) (s : int) : int return = ([]:operation list) , s

(* Tezos.create_contract can't be used in top-level of a view *)
[@view] let bad_view1 (n : int) (s : int) : int =
  let _ = Tezos.create_contract main (None : key_hash option) 0mutez 2 in
  s + n + 1
