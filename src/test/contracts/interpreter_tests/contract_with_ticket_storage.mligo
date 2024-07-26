let assert = Assert.assert

type storage = (bytes ticket) option
type unforged_storage = (bytes unforged_ticket) option

let main (() : unit) (s : storage) : operation list * storage =
  [] , (
    match s with
    | Some ticket ->
      let (_ , t) = Tezos.Next.Ticket.read ticket in
      Some t
    | None -> None
  )

let test_originate_contract =
  let mk_storage = fun (t : bytes ticket) -> Some t in
  let ticket_info = (0x0202, 15n) in
  let addr = Test.Proxy_ticket.originate ticket_info mk_storage main in
  let unforged_storage : unforged_storage = Test.Proxy_ticket.get_storage addr in
  (* the ticket 'unforged_storage' can be manipulated freely without caring about ticket linearity *)

  match unforged_storage with
  | Some { ticketer=_ ; value ; amount } ->
    let () = Test.Next.IO.log ("unforged_ticket", unforged_storage) in
    let () = assert (value = ticket_info.0) in
    let () = assert (amount = ticket_info.1) in
    ()
  | None -> failwith "impossible"
