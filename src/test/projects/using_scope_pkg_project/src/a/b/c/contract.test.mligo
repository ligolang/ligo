#import "contract.mligo" "C"

let test_originate =
  let initial_storage = [1 ; 2 ; 3] in
  let orig = Test.originate (contract_of C) initial_storage 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  let storage = Test.get_storage orig.addr in
  assert (storage = [3 ; 2 ; 1])
