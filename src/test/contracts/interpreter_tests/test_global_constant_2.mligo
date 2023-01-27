type storage = int
type parameter = unit
type return = operation list * storage

let ct : michelson_program = Test.constant_to_michelson_program "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
let ct : string = Test.register_constant ct

let main (() : parameter)  (store : storage) : return =
 ([] : operation list), ((Tezos.constant ct : int -> int) store)

let test =
  let (taddr, _, _) = Test.originate main 1 0tez in
  let ctr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn ctr () 0tez in
  assert (Test.get_storage taddr = 5)
