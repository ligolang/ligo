let (a1, a2, a3) 
  = ( Tezos.create_ticket 1 10n
    , Tezos.create_ticket "one" 10n
    , Tezos.create_ticket 1n 10n
    )

let (b1, b2, b3) 
  = ( Tezos.create_ticket 2 10n
    , Tezos.create_ticket "TWO" 10n
    , Tezos.create_ticket 3n 10n
    )

type storage = int ticket * string ticket * nat ticket

let main (_,_ : unit * storage) : operation list * storage
  = [],
    (let a = Option.unopt (Tezos.join_tickets (a1, b1)) in
    let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
    let c  = Option.unopt (Tezos.join_tickets (a3, b3)) in
    (a, b, c))