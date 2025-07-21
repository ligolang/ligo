let empty (n : nat) : 42 sapling_state =
  let _n = n in
  Tezos.Sapling.empty_state

[@entry]
let main () (s : int) : operation list * int =
  let s4 = empty in
  let () = ignore s4
  in [], s
