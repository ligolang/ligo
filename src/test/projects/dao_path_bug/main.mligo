#import "@ligo/dao/src/token.mligo" "Token"
type storage = nat option

[@entry]
let main () (_ : storage) : operation list * storage =
  [], Token.get_total_supply (Tezos.get_sender ())
