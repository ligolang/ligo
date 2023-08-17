#include "./contract_under_test/contract_record_storage_ty.mligo"
let cut = "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let init_storage = Test.run (fun () -> {foo = 0 ; bar = "bar"}) () in
  let (addr, _code, _size) = Test.originate_from_file cut "main" init_storage 0tez in
  let store = Test.get_storage_of_address addr in
  let store = (Test.decompile store : storage) in
  let ovens_map = store.foo in
  assert (ovens_map = 0)
