#import "contract.mligo" "C"

let test_originate =
  let initial_storage = [1 ; 2 ; 3] in
  let (taddr,_,_) = Test.originate C.main initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn contr () 0tez in
  let storage = Test.get_storage taddr in
  assert (storage = [3 ; 2 ; 1])
