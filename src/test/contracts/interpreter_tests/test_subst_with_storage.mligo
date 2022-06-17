#include "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let init_storage = {foo = 0 ; bar = "bar"} in
  let (addr, _code, _size) = Test.originate main init_storage 0tez in
  let store = Test.get_storage addr in
  Test.eval store.foo
