#include "contract.mligo"

let test_originate =
  let initial_storage = [1 ; 2 ; 3] in
  let v_mich = Test.run (fun (x : storage) -> x) initial_storage in
  let (addr,_,_) = Test.originate_from_file "contract.mligo" "main" ([]: string list) v_mich 0tez in
  let t_addr : (parameter, storage) typed_address = Test.cast_address addr in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr () 0tez in
  let storage = Test.get_storage t_addr in
  assert (storage = [3 ; 2 ; 1])
