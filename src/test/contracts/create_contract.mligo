type return = operation list * string

[@entry]
let main (action : string) (store : string) : return =
  let toto : operation * address =
    Tezos.create_contract
      (fun (p : nat) (s : string) -> (([] : operation list), "one"))
      (None : key_hash option)
      300000000mutez
      "un" in
  ([toto.0], store)
