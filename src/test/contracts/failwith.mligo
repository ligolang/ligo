type storage = unit

let main (p : unit; store : storage) : operation list * storage =
  if true then failwith "This contract always fails"
