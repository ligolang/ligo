type storage = unit
type result = operation list * storage

[@entry]
let main (_param : unit) (_store : storage) : result =
  failwith "This contract always fails."