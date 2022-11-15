let no_operation = ([] : operation list)
type storage = timestamp
type result = operation list * storage

let main (ts, _ : timestamp * storage) : result =
  no_operation, ts 



let boot () = 
  let () = Test.reset_state 2n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 1 in
  let () = Test.set_source sender_ in

  let init_storage = ("2022-01-01t10:10:10Z" : timestamp) in

  let (taddr, _, _) = Test.originate main init_storage 0mutez in
  let contr = Test.to_contract taddr in
  let addr = Tezos.address contr in
  {addr = addr; taddr = taddr; contr = contr}

let test_timestamp = 
  let c = boot() in
  let r = Test.transfer_to_contract c.contr ("2022-01-01t10:10:10Z" : timestamp) 0tez in
  Test.log r

