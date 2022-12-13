type t is record[ b : ticket(string) ]

const record[ b ] = record[ b = Tezos.create_ticket ("one", 10n) ]

type storage is ticket(string)

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, Option.unopt (Tezos.join_tickets (b, b)))