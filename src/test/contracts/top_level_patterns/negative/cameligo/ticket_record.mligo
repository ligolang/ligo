module Tezos = Tezos.Next

type t = {b : string ticket}

let {b} = {b = Option.value_with_error "option is None" (Tezos.Ticket.create "one" 10n)}

type storage = string ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage =
  [], Option.value_with_error "option is None" (Tezos.Ticket.join (b, b))
