#include "./contract_under_test/contract_record_storage_ty.mligo"
let cut = "./contract_under_test/contract_record_storage_ty.mligo"

module Test = Test.Next
module Originate = Test.Originate
let assert = Assert.assert

let test =
  let orig : (parameter, storage) Originate.origination_result =
    Originate.from_file cut {foo = 0 ; bar = "bar"} 0tez in
  let store : storage = Test.Typed_address.get_storage orig.taddr in
  let ovens_map = store.foo in
  assert (ovens_map = 0)
