module Test = Test.Next

module C = struct
  [@entry]
  let main (p : int * int) () =
    let op1 = Tezos.Operation.emit "%foo" p in
    let op2 = Tezos.Operation.emit "%foo" p.0 in
    [op1; op2], ()
end

let test_foo =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let _: nat = Test.Typed_address.transfer_exn orig.taddr (Main (1,2)) 0tez in
  (Test.State.last_events orig.taddr "foo" : (int*int) list),(Test.State.last_events orig.taddr "foo" : int list)