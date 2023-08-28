[@entry]
let main (_ : unit) (_ : nat) : operation list * nat =
  ([], Tezos.get_min_block_time ())
