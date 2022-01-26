type storage = int
type parameter = unit
type return = operation list * storage

let main ((), store : parameter * storage) : return =
 ([] : operation list), ((Tezos.constant "myhash" : int -> int) store)
