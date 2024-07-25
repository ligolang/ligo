module Tezos = Tezos.Next
module Test = Test.Next

module C = struct
  module Storage =
    struct
      type tickets = (address, unit ticket) big_map
      type storage_data = {
          price : tez;
      }
      type t = {data : storage_data; tickets : tickets}
    end

  type storage = Storage.t

  let buy_ticket ({data; tickets} : storage) : operation list * storage =
    let _check_given_amount =
      Assert.Error.assert
        (Tezos.get_amount () = data.price)
        "1" in
    let owner = (Tezos.get_sender()) in
    let (owned_tickets_opt, tickets) =
      Big_map.get_and_update owner (None : unit ticket option) tickets in
    let new_ticket =
      Option.value_with_error "option is None" (Tezos.Ticket.create unit 1n) in
    let join_tickets =
      match owned_tickets_opt with
        None -> new_ticket
      | Some owned_tickets ->
          (match Tezos.Ticket.join (owned_tickets, new_ticket) with
              None -> failwith "2"
              | Some joined_tickets -> joined_tickets)
    in
    let (_, tickets) =
      Big_map.get_and_update owner (Some join_tickets) tickets in
    [], {data = data; tickets = tickets}

  [@entry]
  let main (_ : unit) (store : storage) =
    buy_ticket(store)
end

let test_one =
  let () = Test.State.reset 2n ([] : tez list) in
  let sender_ = Test.Account.address 1 in
  let () = Test.State.set_source sender_ in

  let init_storage = {
      data = {
        price = 1tz;
      };
      tickets = (Big_map.empty: (address, unit ticket) big_map);
  } in

  let orig = Test.Originate.contract (contract_of C) init_storage 0mutez in
  let r = Test.Typed_address.transfer orig.taddr (Main ()) 1tez in
  Test.IO.log (r)
