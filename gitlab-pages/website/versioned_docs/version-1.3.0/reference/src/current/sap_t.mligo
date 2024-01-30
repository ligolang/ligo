type st = 8 sapling_state
type tr = 8 sapling_transaction
let x = Tezos.sapling_empty_state
let f (tr : tr) =
  match Tezos.sapling_verify_update tr x with
    Some (_, x) -> x
  | None -> (failwith "failed" : int * st)