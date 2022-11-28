let main (_,_ : unit * string ) : operation list * string =
  [Tezos.emit "%hello world" 12], "bye"
