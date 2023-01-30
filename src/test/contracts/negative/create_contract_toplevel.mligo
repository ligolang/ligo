type return = operation list * string

let main (_ : string) (store : string) : return =
  let toto : operation * address = Tezos.create_contract
    (fun (_p : nat) (_s : string) -> (([] : operation list), store))
    (None: key_hash option) 
    300tz 
    "un"
  in
  ([toto.0], store)
