module Tezos = Tezos.Next

let foo : int = 42

type return = operation list * int

let dummy_contract (_p : nat) (_s : int) : return = [], foo

[@entry]
let main (_action : int) (_store : int) : return =
  let op, _addr =
    Tezos.Operation.create_contract
      dummy_contract
      ((None : key_hash option))
      300000000mutez
      1
  in [op], foo
