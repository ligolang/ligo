type t = { b : string ticket }

let { b } = { b = Tezos.create_ticket "one" 10n }

type storage = string ticket

let main (_,_ : unit * storage) : operation list * storage
  = [], Option.unopt (Tezos.join_tickets (b, b))