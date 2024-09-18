module MyContract = struct
  type storage = int
  type param = int * int ticket

  [@entry] let main (param : param) (storage : storage) : operation list * storage =
    let (multiplier, ticket) = param in
    // Read the ticket, destroy it, and add its amount times the multiplier to storage
    let ((_address, (payload, amount)), _ticket) = Tezos.Next.Ticket.read ticket in
    [], (storage + (multiplier * payload * (int amount)))
end
let test_transfer_to_contract =
  // Originate the contract as usual
  let orig = Test.Next.Originate.contract (contract_of MyContract) 0 0tez in
  let main_addr = Test.Next.Typed_address.to_address orig.taddr in

  // Create a function that the proxy runs to return the parameter
  let create_param : int ticket -> MyContract.param = fun (t : int ticket) -> 5, t in

  // Create the proxy contract
  let proxy_taddr = Test.Proxy_ticket.init_transfer create_param in
  let () = Test.Next.IO.log ("proxy addr:", proxy_taddr) in

  // Use the proxy to call the entrypoint
  let ticket_info = 3, 10n in
  let _ : test_exec_result = Test.Proxy_ticket.transfer proxy_taddr (ticket_info, main_addr) in
  Assert.assert ((Test.Next.Typed_address.get_storage orig.taddr) = 150)