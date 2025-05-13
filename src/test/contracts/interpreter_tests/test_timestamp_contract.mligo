module Test = Test.Next

module C = struct
  [@entry]
  let main (ts : timestamp) (_ : timestamp) : operation list * timestamp = [], ts
end


let boot () =
  let () = Test.State.reset 2n ([] : tez list) in
  let sender_ = Test.Account.address 1 in
  let () = Test.State.set_source sender_ in

  let init_storage = ("2022-01-01t10:10:10Z" : timestamp) in

  let orig = Test.Originate.contract (contract_of C) init_storage 0mutez in
  let contr = Test.Typed_address.to_contract orig.taddr in
  let addr = Tezos.address contr in
  {addr = addr; taddr = orig.taddr; contr = contr}

let test_timestamp =
  let c = boot() in
  let r = Test.Contract.transfer c.contr (Main ("2022-01-01t10:10:10Z" : timestamp)) 0tez in
  Test.IO.log r
