let main (_ : unit * nat ) : operation list * nat =
  ([],Tezos.get_min_block_time ())