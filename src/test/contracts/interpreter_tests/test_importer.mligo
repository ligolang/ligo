#import "test_imported.mligo" "External"

let test =
  let orig = Test.originate (contract_of External) External.D.default.initial 0tez in
  let contr = Test.to_contract orig.addr in
  let () = assert (Test.get_storage orig.addr = External.D.default.initial) in
  let _ = Test.transfer_to_contract_exn contr (Main (External.D.default.final)) 0tez in
  assert (Test.get_storage orig.addr = External.D.default.final)
