const (a1, a2, a3) 
  = ( Tezos.create_ticket (1, 10n)
    , Tezos.create_ticket ("one", 10n)
    , Tezos.create_ticket (1n, 10n)
    )

const (b1, b2, b3) 
  = ( Tezos.create_ticket (2, 10n)
    , Tezos.create_ticket ("TWO", 10n)
    , Tezos.create_ticket (3n, 10n)
    )

type storage is ticket(int) * ticket(string) * ticket(nat)

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is {
    var a := Option.unopt (Tezos.join_tickets (a1, b1));
    var b := Option.unopt (Tezos.join_tickets (a2, b2));
    var c := Option.unopt (Tezos.join_tickets (a3, b3));
  } with (nil, (a, b, c))