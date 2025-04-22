module Tezos = Tezos.Next
module Proxy_ticket = Test.Proxy_ticket
module Test = Test.Next

type storage = (bytes ticket) option
type unforged_storage = (bytes unforged_ticket) option

let main (() : unit) (s : storage) : operation list * storage =
  [] , (
    match s with
    | Some ticket ->
      let (_ , t) = Tezos.Ticket.read ticket in
      Some t
    | None -> None
  )

let test_originate_contract =
  let mk_storage = fun (t : bytes ticket) -> Some t in
  let ticket_info = (0x0202, 15n) in
  let addr = Proxy_ticket.originate ticket_info mk_storage main in
  let unforged_storage : unforged_storage = Proxy_ticket.get_storage addr in
  (* the ticket 'unforged_storage' can be manipulated freely without caring about ticket linearity *)

  match unforged_storage with
  | Some { ticketer=_ ; value ; amount } ->
    let () = Test.IO.log ("unforged_ticket", unforged_storage) in
    let () = Assert.assert (value = ticket_info.0) in
    let () = Assert.assert (amount = ticket_info.1) in
    ()
  | None -> failwith "impossible"
