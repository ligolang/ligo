[@entry]
let main () (_ : string) : operation list * string =
  let x = "%lol" in
  [Tezos.emit x 12], x
