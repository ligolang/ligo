#import "./contract_under_test/views_contract.mligo" "CUT"
let test =
  let _baker = Test.nth_bootstrap_account 0 in
  let _src = Test.nth_bootstrap_account 1 in
  let init_storage = 0 in
  let {addr = addr_v ; code = _ ; size = _} = Test.originate (contract_of CUT.Main_with_view) init_storage 0mutez in
  let {addr = addr_c ; code = _ ; size = _} = Test.originate (contract_of CUT.Caller) init_storage 0mutez in
  let tx =
    Test.transfer
      addr_c
      (Main (Test.to_address addr_v))
      1tez in
  match tx with
    Success _ ->
      let x = Test.get_storage addr_c in
      assert (x = 2)
  | _ -> failwith "transfer to view caller failed"
