type storage = timestamp

let main (p : unit) (s : storage) : operation list * storage =
  [], Tezos.get_now ()
