type t = { a : ticket(int) , b : ticket(string) , c : ticket(nat) }

let { a : a1 , b : b1 , c : c1 }
    = { a : Tezos.create_ticket (1, 10n)
      , b : Tezos.create_ticket ("one", 10n)
      , c : Tezos.create_ticket (1n, 10n)
      }

let { a : a2 , c : c2 , b : b2 }
    = { a : Tezos.create_ticket (2, 10n)
      , b : Tezos.create_ticket ("TWO", 10n)
      , c : Tezos.create_ticket (3n, 10n)
      }

type storage = (ticket(int) , ticket(string) , ticket(nat))

let main = ((_,_) : (unit , storage)) : (list(operation) , storage)
  => ([],
    (let a = Option.unopt (Tezos.join_tickets (a1, a2));
    let b  = Option.unopt (Tezos.join_tickets (b1, b2));
    let c  = Option.unopt (Tezos.join_tickets (c1, c2));
    (a, b, c)))