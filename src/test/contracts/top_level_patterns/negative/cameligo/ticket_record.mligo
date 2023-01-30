type t = { b : string ticket }

let { b } = { b = Option.unopt (Tezos.create_ticket "one" 10n) }

type storage = string ticket

let main (_ : unit) (_ : storage) : operation list * storage
  = [], Option.unopt (Tezos.join_tickets (b, b))