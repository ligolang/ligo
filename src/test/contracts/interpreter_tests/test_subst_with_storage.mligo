#import "./contract_under_test/contract_record_storage_ty.mligo" "C"

module Test = Test.Next

let test =
  let init_storage = {foo = 0 ; bar = "bar"} in
  let orig = Test.Originate.contract (contract_of C) init_storage 0tez in
  let store = Test.Typed_address.get_storage orig.taddr in
  Test.Michelson.eval store.foo
