const (b, _) = (Option.unopt (Tezos.create_ticket ("one", 10n)), 1)

type storage is ticket(string)

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, Option.unopt (Tezos.join_tickets (b, b)))