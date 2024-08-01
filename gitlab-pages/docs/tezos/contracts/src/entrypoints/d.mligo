(* proxy.mligo *)

type parameter =
  Increment of int
| Decrement of int
| Reset

type storage = unit

type result = operation list * storage

let dest = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address)

[@entry]
let proxy (action : parameter) (store : storage) : result =
  let counter : parameter contract = Tezos.get_contract_with_error dest "not found" in
  let op = Tezos.transaction (Increment 5) 0tez counter
  in [op], store