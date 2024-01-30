module C = struct
  type param = int * string ticket
  type storage = string * address

  [@entry]
  let main (p : param) (_ : storage) : operation list * storage =
    let (_,ticket) = p in
    let (_,(v,_)) , _ = Tezos.read_ticket ticket in
    [] , (v, Tezos.get_sender ())
end
let test_transfer_to_contract =
  let {addr = main_taddr; code = _ ; size = _} = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
  let main_addr = Test.to_address main_taddr in

  (* Use this address everytime you want to send tickets from the same proxy-contract *)
  let proxy_taddr =
    (* mk_param is executed __by the proxy contract__ *)
    let mk_param : string ticket -> C.param = fun (t : string ticket) -> 42,t in
    (* initialize a proxy contract in charge of creating and sending your tickets *)
    Test.Proxy_ticket.init_transfer mk_param
  in
  let () = Test.log ("poxy addr:", proxy_taddr) in

  let _ =
    (* ticket_info lets you control the amount and the value of the tickets you send *)
    let ticket_info = ("hello", 10n) in
    (* we send ticket to C through the proxy-contract *)
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let () = Test.log (Test.get_storage main_taddr) in
  let _ =
    let ticket_info = ("world",5n) in
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let () = Test.log (Test.get_storage main_taddr) in
  ()