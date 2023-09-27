#import "C.mligo" "C"

let test =
  let orig = Test.originate (contract_of C) () 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  assert (Test.get_storage orig.addr = ())
