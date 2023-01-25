type storage = timestamp

let main (p, s : unit * storage) : operation list * storage =
  [], Tezos.get_now ()
