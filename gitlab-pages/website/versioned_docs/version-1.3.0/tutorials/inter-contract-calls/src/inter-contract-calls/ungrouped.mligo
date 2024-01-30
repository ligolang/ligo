type parameter = address

type storage = unit

[@entry]
let main (destination_addr : parameter) (_ : storage) =
  let maybe_contract = Tezos.get_contract_opt destination_addr in
  let destination_contract =
    match maybe_contract with
      Some contract -> contract
    | None -> failwith "Contract does not exist" in
  let op = Tezos.transaction () (Tezos.get_amount ()) destination_contract in
  [op], ()
type t = {hello : int; l : nat; i : bytes; g : string; o : address}
(* examples/contracts/mligo/CreateAndCall.mligo *)

// Here we create two operations: the one that will originate
// the contract, and an operation to self, that will continue
// the execution after the contract is originated.

let create_and_call (storage : address list) =
  let create_op, addr =
    Tezos.create_contract
      (fun (p : int) (s : int) -> [], p + s)
      None
      0tez
      1 in
  let call_op =
    Tezos.transaction (addr, 41) 0tez (Tezos.self "%callback") in
  [create_op; call_op], addr :: storage