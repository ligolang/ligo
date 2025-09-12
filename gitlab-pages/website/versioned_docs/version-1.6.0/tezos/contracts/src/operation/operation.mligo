type return = operation list * string

[@entry]
let main (_ : string) (storage : string) : return =
  let entrypoint (_ : nat) (storage : string) =
    (([] : operation list), storage) in
  let op, _addr : operation * address =
    Tezos.create_contract
      entrypoint
      (None : key_hash option)
      300000000mutez
      "one"
  in [op], storage