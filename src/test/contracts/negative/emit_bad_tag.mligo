[@entry]
let main (_ : unit) (_ : string) : operation list * string =
  [Tezos.Operation.emit "%hello world" 12], "bye"
