type mystruct = {
  num1 : nat;
  num2 : nat;
}

type storage = unit

let main (p,s: mystruct * storage ) : operation list * storage = [ Tezos.emit "%foo" p; ], s

let test_foo =
  let (ta, _, _) = Test.originate main () 0tez in
  let _ = Test.transfer_to_contract_exn (Test.to_contract ta) {num1 = 1n ; num2 = 2n} 0tez in
  let events = (Test.get_last_events_from (ta: (mystruct,storage) typed_address) "foo" : mystruct list) in
  match events with [{num1;num2}] -> num1 + num2 | _ -> Test.failwith "not good"
