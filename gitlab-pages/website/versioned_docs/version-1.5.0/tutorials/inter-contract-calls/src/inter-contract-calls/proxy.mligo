(* examples/contracts/mligo/Proxy.mligo *)

type parameter = int

type storage = address

let get_contract (addr : address) =
  match Tezos.get_contract_opt addr with
    Some contract -> contract
  | None -> failwith "Callee does not exist"

[@entry]
let main (param : parameter) (callee_addr : storage) =
  let callee = get_contract (callee_addr) in
  let op = Tezos.transaction param 0mutez callee in
  [op], callee_addr