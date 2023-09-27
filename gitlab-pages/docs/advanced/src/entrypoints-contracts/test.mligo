#import "incdec.mligo" "C"

let test =
  let (ta, _, _) = Test.originate (contract_of C.IncDec) 0 (0tez) in
  let c : C.IncDec parameter_of contract = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Increment 42) (0tez) in
  assert (42 = Test.get_storage(ta))