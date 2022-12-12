let (b, _) = (Tezos.create_ticket "one" 10n, 1)

type storage = string ticket

let main (_,_ : unit * storage) : operation list * storage
  = [], Option.unopt (Tezos.join_tickets (b, b))