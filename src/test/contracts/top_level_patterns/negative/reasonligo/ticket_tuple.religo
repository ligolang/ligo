let (b, _) = (Option.unopt (Tezos.create_ticket ("one", 10n)), 1)

type storage = ticket(string)

let main = ((_,_) : (unit , storage)) : (list(operation) , storage)
  => ([], Option.unopt (Tezos.join_tickets (b, b)))