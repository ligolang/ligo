(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_builder_fungible.tz

Goes with ticket_wallet.mligo.
*)

module Tezos = Tezos.Next

type mint_parameter =
  [@layout comb]
  {
   destination : unit ticket contract;
   amount : nat
  }

type parameter =
| Burn of unit ticket
| Mint of mint_parameter

type storage = [@layout comb] {admin : address}

[@entry]
let main (p : parameter) (s : storage) : operation list * storage =
  begin
    Assert.assert (Tezos.get_amount () = 0mutez);
    match p with
      Burn ticket ->
        begin
          let ((ticketer, _), ticket) =
            (Tezos.Ticket.read ticket : (address * (unit * nat)) * unit ticket) in
          Assert.assert (ticketer = Tezos.get_self_address ());
          (([] : operation list), s)
        end
    | Mint mint ->
        begin
          Assert.assert (Tezos.get_sender () = s.admin);
          let ticket =
            Option.value_with_error
              "option is None" (Tezos.Ticket.create () mint.amount) in
          let op = Tezos.Operation.transaction ticket 0mutez mint.destination in
          ([op], s)
        end
  end
