(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_wallet_fungible.tz

Goes with ticket_builder.mligo.
*)

module Tezos = Tezos.Next

type send_parameter =
  [@layout comb]
  {
   destination : unit ticket contract;
   amount : nat;
   ticketer : address
  }

type parameter =
| Receive of unit ticket
| Send of send_parameter

type storage =
  [@layout comb]
  {
   manager : address;
   tickets : (address, unit ticket) big_map
  }

[@entry]
let main (p : parameter) (storage : storage) : operation list * storage =
  begin
    Assert.assert (Tezos.get_amount () = 0mutez);
    let {
     manager = manager;
     tickets = tickets
    } = storage in
    (match p with
       Receive ticket ->
         let ((ticketer, _), ticket) = Tezos.Ticket.read ticket in
         let (old_ticket, tickets) =
           Big_map.get_and_update ticketer (None : unit ticket option) tickets in
         let ticket =
           match old_ticket with
             None -> ticket
           | Some old_ticket ->
               (match Tezos.Ticket.join (ticket, old_ticket) with
                  None -> (failwith "impossible?" : unit ticket)
                | Some joined -> joined) in
         let (_, tickets) =
           Big_map.get_and_update ticketer (Some ticket) tickets in
         (([] : operation list),
          {
           manager = manager;
           tickets = tickets
          })
     | Send send ->
         begin
           Assert.assert (Tezos.get_sender () = manager);
           let (ticket, tickets) =
             Big_map.get_and_update
               send.ticketer
               (None : unit ticket option)
               tickets in
           (match ticket with
              None -> (failwith "no tickets" : operation list * storage)
            | Some ticket ->
                let ((_, (_, total_amt)), ticket) = Tezos.Ticket.read ticket in
                let send_amt = send.amount in
                let keep_amt : nat =
                  match is_nat (total_amt - send_amt) with
                    None -> (failwith "not enough tickets" : nat)
                  | Some keep_amt -> keep_amt in
                (match Tezos.Ticket.split ticket (send_amt, keep_amt) with
                   None -> (failwith "impossible?" : operation list * storage)
                 | Some split_tickets ->
                     let (send_ticket, keep_ticket) = split_tickets in
                     let (_, tickets) =
                       Big_map.get_and_update
                         send.ticketer
                         (Some keep_ticket)
                         tickets in
                     let op =
                       Tezos.Operation.transaction send_ticket 0mutez send.destination in
                     ([op],
                      {
                       manager = manager;
                       tickets = tickets
                      })))
         end)
  end
