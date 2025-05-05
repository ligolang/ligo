module Tezos = Tezos.Next

type return = operation list * string

[@entry]
let main (_ : string) (store : string) : return =
  [@thunk]
  let sender = Tezos.get_sender () in
  let toto : operation * address =
    Tezos.Operation.create_contract
      (fun (_ : nat) (_ : address) -> (([] : operation list), sender))
      (None : key_hash option)
      1000000mutez
      sender in
  ([toto.0], store)
