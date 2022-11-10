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
    assert_with_error
      (Tezos.get_amount () = data.price)
      "1" in
  let owner = (Tezos.get_sender()) in
  let (owned_tickets_opt, tickets) =
    Big_map.get_and_update owner (None : unit ticket option) tickets in
  let new_ticket = Option.unopt (Tezos.create_ticket unit 1n) in
  let join_tickets =
    match owned_tickets_opt with
      None -> new_ticket
    | Some owned_tickets ->
        (match Tezos.join_tickets (owned_tickets, new_ticket) with
            None -> failwith "2"
            | Some joined_tickets -> joined_tickets)
  in
  let (_, tickets) =
    Big_map.get_and_update owner (Some join_tickets) tickets in
  [], {data = data; tickets = tickets}

let main (_, store : unit * storage) =
  buy_ticket(store)


let test_one = 
  let () = Test.reset_state 2n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 1 in
  let () = Test.set_source sender_ in

  let init_storage = {
      data = {
        price = 1tz;
      };
      tickets = (Big_map.empty: (address, unit ticket) big_map);
  } in

  let (taddr, _, _) = Test.originate main init_storage 0mutez in
  let contr = Test.to_contract taddr in
  let r = Test.transfer_to_contract contr () 1tez in
  Test.log (r)
