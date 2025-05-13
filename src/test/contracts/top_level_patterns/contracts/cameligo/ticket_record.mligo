module Ticket = Tezos.Ticket

type t = { a : int ticket ; b : string ticket ; c : nat ticket }

let { a = a1 ; b = b1 ; c = c1 }
    = { a = Option.value_with_error "option is None" (Ticket.create 1 10n)
      ; b = Option.value_with_error "option is None" (Ticket.create "one" 10n)
      ; c = Option.value_with_error "option is None" (Ticket.create 1n 10n)
      }

let { a = a2 ; c = c2 ; b = b2 }
    = { a = Option.value_with_error "option is None" (Ticket.create 2 10n)
      ; b = Option.value_with_error "option is None" (Ticket.create "TWO" 10n)
      ; c = Option.value_with_error "option is None" (Ticket.create 3n 10n)
      }

type storage = int ticket * string ticket * nat ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage
  = [],
    (let a = Option.value_with_error "option is None" (Ticket.join (a1, a2)) in
    let b  = Option.value_with_error "option is None" (Ticket.join (b1, b2)) in
    let c  = Option.value_with_error "option is None" (Ticket.join (c1, c2)) in
    (a, b, c))
