module Ticket = Tezos.Ticket

let my_ticket1 = Option.value_with_error "ticket1 failed" (Ticket.create 1 10n)
let my_ticket2 = Option.value_with_error "ticket2 failed" (Ticket.create "one" 10n)
let v =
  let (_addr, (payload, _amt)), _ticket = Ticket.read my_ticket1
  in payload
let ta, tb =
  match Ticket.split my_ticket1 (6n, 4n) with
    None -> failwith "amt_a + amt_v <> amt"
  | Some split_tickets -> split_tickets