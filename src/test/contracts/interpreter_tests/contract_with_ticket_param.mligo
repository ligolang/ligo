module Test = Test.Next

module C = struct
  type param = int * string ticket

  [@entry]
  let main (p : param) (_ : string * address) : operation list * (string * address) =
    let (_,ticket) = p in
    let (_,(v,_)) , _ = Tezos.Ticket.read ticket in
    [] , (v, Tezos.get_sender ())
end

let test_transfer_to_contract =
  let orig = Test.Originate.contract (contract_of C)
               ("bye", Test.Account.address 1) 1mutez in
  let main_addr = Tezos.address (Test.Typed_address.to_contract orig.taddr) in

  (* Use this address everytime you want to send tickets from the same proxy-contract *)
  let proxy_taddr =
    (* mk_param is executed __by the proxy contract__ *)
    let mk_param : string ticket -> C.param = fun (t : string ticket) -> 42,t in
    (* initialize a proxy contract in charge of creating and sending your tickets *)
    Test.Ticket.Proxy.init_transfer mk_param
  in
  let _ =
    (* ticket_info lets you control the amount and the value of the tickets you send *)
    let ticket_info = ("hello",10n) in
    (* we send ticket to main through the proxy-contract *)
    Test.Ticket.Proxy.transfer proxy_taddr (ticket_info,main_addr)
  in
  let _ =
    let ticket_info = ("world",5n) in
    Test.Ticket.Proxy.transfer proxy_taddr (ticket_info,main_addr)
  in
  let s, addr = Test.Address.get_storage main_addr in
  let p_addr = proxy_taddr |> Test.Typed_address.to_contract |> Tezos.address in
  Assert.assert (s = "world" && addr = p_addr)
