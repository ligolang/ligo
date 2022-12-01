let (a1, a2, a3) 
  = ( Tezos.create_ticket (1, 10n)
    , Tezos.create_ticket ("one", 10n)
    , Tezos.create_ticket (1n, 10n)
    )

let (b1, b2, b3) 
  = ( Tezos.create_ticket (2, 10n)
    , Tezos.create_ticket ("TWO", 10n)
    , Tezos.create_ticket (3n, 10n)
    )

type storage = (ticket(int) , ticket(string) , ticket(nat))

let main = ((_,_) : (unit , storage)) : (list(operation) , storage)
  => ([],
    (let a = Option.unopt (Tezos.join_tickets (a1, b1));
    let b  = Option.unopt (Tezos.join_tickets (a2, b2));
    let c  = Option.unopt (Tezos.join_tickets (a3, b3));
    (a, b, c)))