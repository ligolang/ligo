type t = { a : int ticket ; b : string ticket ; c : nat ticket }

let { a = a1 ; b = b1 ; c = c1 }
    = { a = Option.unopt (Tezos.create_ticket 1 10n)
      ; b = Option.unopt (Tezos.create_ticket "one" 10n)
      ; c = Option.unopt (Tezos.create_ticket 1n 10n)
      }

let { a = a2 ; c = c2 ; b = b2 }
    = { a = Option.unopt (Tezos.create_ticket 2 10n)
      ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
      ; c = Option.unopt (Tezos.create_ticket 3n 10n)
      }

type storage = int ticket * string ticket * nat ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage
  = [],
    (let a = Option.unopt (Tezos.join_tickets (a1, a2)) in
    let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
    let c  = Option.unopt (Tezos.join_tickets (c1, c2)) in
    (a, b, c))