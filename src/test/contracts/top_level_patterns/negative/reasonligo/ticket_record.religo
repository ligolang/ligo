type t = { b : ticket(string) }

let { b } = { b : Tezos.create_ticket ("one", 10n) }

type storage = ticket(string)

let main = ((_,_) : (unit , storage)) : (list(operation) , storage)
  => ([], Option.unopt (Tezos.join_tickets (b, b)))