module MyContract = struct

  type storage = int * (bytes ticket) option
  type unforged_storage = int * (bytes unforged_ticket) option

  [@entry] let main (_ : unit) (storage : storage) :operation list * storage =
    let (stored_value, ticket_opt) = storage in
    let new_storage : storage =
      match ticket_opt with
        // If there is a ticket, add its amount to the int in storage
        Some ticket ->
          let ((_address, (_payload, amount)), new_ticket) = Tezos.Next.Ticket.read ticket in
          (stored_value + (int amount), Some new_ticket)
        // If there is no ticket in storage, do nothing
        | None -> stored_value, None ()
      in
    [], new_storage

end
let test_originate_contract =

  // Create a function that the proxy runs to return the contract storage
  let create_storage = fun (t : bytes ticket) : MyContract.storage -> (0, Some t) in
  let ticket_bytes : bytes = 0x0202 in
  let ticket_amount = 15n in
  let ticket_info = ticket_bytes, ticket_amount in

  // Create the proxy contract and use it to originate the contract
  let addr = Test.Proxy_ticket.originate ticket_info create_storage MyContract.main in
  // ...
  // The ticket 'unforged_ticket_opt' can be manipulated freely without being destroyed
  let unforged_storage : MyContract.unforged_storage = Test.Proxy_ticket.get_storage addr in
  let (_stored_value, unforged_ticket_opt) = unforged_storage in

  // Verify that the ticket is in storage
  let () = match unforged_ticket_opt with
    Some unforged_ticket ->
      let () = Test.Next.IO.log ("unforged_ticket",unforged_ticket) in
      let { ticketer ; value ; amount } = unforged_ticket in
      let () = Assert.assert (value = ticket_bytes) in
      Assert.assert (amount = ticket_amount)
    | None -> failwith "impossible"
    in

  // Call the entrypoint and verify that the value in storage changes
  let _ : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint"default" addr) unit 0tez in
  let new_storage : MyContract.unforged_storage = Test.Proxy_ticket.get_storage addr in
  let (new_stored_value, _unforged_ticket_opt) = new_storage in
  Assert.assert (new_stored_value = (int ticket_amount))