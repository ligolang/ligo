type t is record[ a : ticket(int) ; b : ticket(string) ; c : ticket(nat) ]

const record[ a = a1 ; b = b1 ; c = c1 ]
    = record
      [ a = Option.unopt (Tezos.create_ticket (1, 10n))
      ; b = Option.unopt (Tezos.create_ticket ("one", 10n))
      ; c = Option.unopt (Tezos.create_ticket (1n, 10n))
      ]

const record[ a = a2 ; c = c2 ; b = b2 ]
    = record
      [ a = Option.unopt (Tezos.create_ticket (2, 10n))
      ; b = Option.unopt (Tezos.create_ticket ("TWO", 10n))
      ; c = Option.unopt (Tezos.create_ticket (3n, 10n))
      ]

type storage is ticket(int) * ticket(string) * ticket(nat)

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is {
    var a := Option.unopt (Tezos.join_tickets (a1, a2));
    var b := Option.unopt (Tezos.join_tickets (b1, b2));
    var c := Option.unopt (Tezos.join_tickets (c1, c2));
  } with(nil, (a, b, c))
