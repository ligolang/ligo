module Tezos = Tezos.Next
module Ticket = Tezos.Ticket

let tc : int ticket option =
  let ta = Option.value_with_error "ta failed" (Ticket.create 1 10n) in
  let tb = Option.value_with_error "tb failed" (Ticket.create 1 5n) in
  Ticket.join (ta, tb)