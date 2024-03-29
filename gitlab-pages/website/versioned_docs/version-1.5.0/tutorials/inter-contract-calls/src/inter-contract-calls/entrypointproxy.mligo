(* contracts/examples/mligo/EntrypointProxy.mligo *)

type parameter = int

type storage = address

let get_add_entrypoint (addr : address) =
  match Tezos.get_entrypoint_opt "%add" addr with
    Some contract -> contract
  | None -> failwith "The entrypoint does not exist"

[@entry]
let main (param : parameter) (callee_addr : storage) =
  let add : int contract = get_add_entrypoint (callee_addr) in
  let op = Tezos.transaction param 0mutez add in
  [op], callee_addr