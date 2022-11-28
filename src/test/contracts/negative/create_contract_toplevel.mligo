type return = operation list * string

let main (_, store : string * string) : return =
  let toto : operation * address = Tezos.create_contract
    (fun (_p, _s : nat * string) -> (([] : operation list), store)) 
    (None: key_hash option) 
    300tz 
    "un"
  in
  ([toto.0], store)