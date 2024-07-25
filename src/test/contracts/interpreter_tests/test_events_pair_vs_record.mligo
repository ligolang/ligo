module Test = Test.Next

let failwith = Test.Assert.failwith

module C = struct
  type mystruct = {
    num1 : nat;
    num2 : nat;
  }

  type storage = unit

  [@entry]
  let main (p : mystruct) (s : storage) : operation list * storage = [ Tezos.emit "%foo" p; ], s
end

let test_foo =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main {num1 = 1n ; num2 = 2n}) 0tez in
  let events = (Test.State.last_events orig.taddr "foo" : C.mystruct list) in
  match events with [{num1;num2}] -> num1 + num2 | _ -> failwith "not good"
