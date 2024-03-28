type storage = unit

[@entry]
let main (param : int * int) () : operation list * storage =
  [Tezos.emit "%foo" param; Tezos.emit "%bar" param.0], ()