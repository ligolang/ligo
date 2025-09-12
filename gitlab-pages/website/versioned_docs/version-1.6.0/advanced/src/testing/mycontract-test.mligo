(* This is mycontract-test.mligo *)

#import "gitlab-pages/docs/advanced/src/testing/mycontract.mligo" "MyContract"
type param = MyContract.C parameter_of

let test1 =
  let initial_storage = 42 in
  let {addr ; code = _ ; size = _} = Test.originate (contract_of MyContract.C) initial_storage 0tez in
  assert (Test.get_storage addr = initial_storage)
(* This continues mycontract-test.mligo *)

let test2 =
  let initial_storage = 42 in
  let orig = Test.originate (contract_of MyContract.C) initial_storage 0tez in
  let gas_cons = Test.transfer_exn orig.addr (Increment (1)) 1mutez in
  let () = Test.log ("gas consumption",gas_cons) in
  assert (Test.get_storage orig.addr = initial_storage + 1)