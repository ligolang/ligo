type storage = int
type parameter = unit
type return = operation list * storage

let f (x : int) = x * 3 + 2

let ct : string = Test.register_constant (Test.eval f)

let main (() : parameter) (store : storage) : return =
 ([] : operation list), ((Tezos.constant ct : int -> int) store)

let test =
  let (taddr, _, _) = Test.originate main 1 0tez in
  let ctr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn ctr () 0tez in
  assert (Test.get_storage taddr = 5)
