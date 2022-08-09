let main (_,_ : unit * string ) : operation list * string =
  let x = "%lol" in
  [Tezos.emit x 12],x