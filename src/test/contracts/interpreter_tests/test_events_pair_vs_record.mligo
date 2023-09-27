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
  let orig = Test.originate (contract_of C) () 0tez in
  let _ = Test.transfer_exn orig.addr (Main {num1 = 1n ; num2 = 2n}) 0tez in
  let events = (Test.get_last_events_from orig.addr "foo" : C.mystruct list) in
  match events with [{num1;num2}] -> num1 + num2 | _ -> Test.failwith "not good"
