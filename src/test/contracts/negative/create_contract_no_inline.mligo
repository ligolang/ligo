let foo : int = 42

type return = operation list * int

let dummy_contract (p : nat) (s : int) : return =
 (([] : operation list), foo)

let main (action : int) (store : int) : return =
  let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
  let toto : operation list = [ op ] in
  (toto, foo)
