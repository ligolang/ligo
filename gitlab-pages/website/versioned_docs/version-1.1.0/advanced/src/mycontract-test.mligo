(* This is mycontract-test.mligo *)

#import "mycontract.mligo" "MyContract"
type param = MyContract parameter_of

let test1 =
  let initial_storage = 42 in
  let taddr, _, _ = Test.originate_module (contract_of MyContract) initial_storage 0tez in
  assert (Test.get_storage taddr = initial_storage)

(* This continues mycontract-test.mligo *)

let test2 =
  let initial_storage = 42 in
  let taddr, _, _ = Test.originate_module (contract_of MyContract) initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let gas_cons = Test.transfer_to_contract_exn contr (Increment (1)) 1mutez in
  let () = Test.log ("gas consumption",gas_cons) in
  assert (Test.get_storage taddr = initial_storage + 1)