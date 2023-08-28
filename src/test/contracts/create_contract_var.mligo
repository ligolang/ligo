type return = operation list * string

let a : int = 2

[@entry]
let main (action : string) (store : string) : return =
  let toto : operation * address =
    Tezos.create_contract
      (fun (p : nat) (s : int) -> (([] : operation list), a))
      (None : key_hash option)
      300000000mutez
      1 in
  ([toto.0], store)
