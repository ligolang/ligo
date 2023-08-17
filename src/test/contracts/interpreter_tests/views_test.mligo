let cut = "./contract_under_test/views_contract.mligo"

let test =
  let _baker = Test.nth_bootstrap_account 0 in
  let _src = Test.nth_bootstrap_account 1 in
  let init_storage = Test.eval 0 in
  let (addr_v, _, _) = Test.originate_from_file cut "main_with_view" init_storage 0tez in
  let (addr_c, _, _) = Test.originate_from_file cut "main_calling" init_storage 0tez in
  match Test.transfer addr_c (Test.eval addr_v) 1tez with
  | Success _ ->
    let x = Test.get_storage (Test.cast_address addr_c : (address,int) typed_address) in
    assert (x = 2)
  | _ -> failwith "transfer to view caller failed"