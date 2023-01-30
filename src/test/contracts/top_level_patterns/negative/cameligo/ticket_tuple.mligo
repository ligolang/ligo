let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)

type storage = string ticket

let main (_ : unit) (_ : storage) : operation list * storage
  = [], Option.unopt (Tezos.join_tickets (b, b))