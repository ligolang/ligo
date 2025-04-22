module Tezos = Tezos.Next

let b, _ = Option.value_with_error "option is None" (Tezos.Ticket.create "one" 10n), 1

type storage = string ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage =
  [], Option.value_with_error "option is None" (Tezos.Ticket.join (b, b))
