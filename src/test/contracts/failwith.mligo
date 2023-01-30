type storage = unit

let main (_ : unit) (_ : storage) : operation list * storage =
  (failwith "This contract always fails" : operation list * storage)
