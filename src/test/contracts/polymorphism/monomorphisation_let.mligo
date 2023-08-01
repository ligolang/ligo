let empty (n : nat) : 42 sapling_state = let _n = n in Tezos.sapling_empty_state

let main (_, s : unit * int) : operation list * int =
  let s4 = empty in
  let () = ignore s4 in
  (([] : operation list), s)
