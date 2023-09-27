module C = struct
  type param = int * string ticket

  [@entry]
  let main (p : param) (_ : string * address) : operation list * (string * address) =
    let (_,ticket) = p in
    let (_,(v,_)) , _ = Tezos.read_ticket ticket in
    [] , (v, Tezos.get_sender ())
end

let test_transfer_to_contract =
  let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
  let main_addr = Tezos.address (Test.to_contract orig.addr) in

  (* Use this address everytime you want to send tickets from the same proxy-contract *)
  let proxy_taddr =
    (* mk_param is executed __by the proxy contract__ *)
    let mk_param : string ticket -> C.param = fun (t : string ticket) -> 42,t in
    (* initialize a proxy contract in charge of creating and sending your tickets *)
    Test.Proxy_ticket.init_transfer mk_param
  in
  let _ =
    (* ticket_info lets you control the amount and the value of the tickets you send *)
    let ticket_info = ("hello",10n) in
    (* we send ticket to main through the proxy-contract *)
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let _ =
    let ticket_info = ("world",5n) in
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let s, addr = Test.get_storage_of_address main_addr in
  let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
  assert (s = "world" && addr = p_addr)