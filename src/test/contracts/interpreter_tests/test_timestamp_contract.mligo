module C = struct
  [@entry]
  let main (ts : timestamp) (_ : timestamp) : operation list * timestamp = [], ts 
end


let boot () = 
  let () = Test.reset_state 2n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 1 in
  let () = Test.set_source sender_ in

  let init_storage = ("2022-01-01t10:10:10Z" : timestamp) in

  let orig = Test.originate (contract_of C) init_storage 0mutez in
  let contr = Test.to_contract orig.addr in
  let addr = Tezos.address contr in
  {addr = addr; taddr = orig.addr; contr = contr}

let test_timestamp = 
  let c = boot() in
  let r = Test.transfer_to_contract c.contr (Main ("2022-01-01t10:10:10Z" : timestamp)) 0tez in
  Test.log r

