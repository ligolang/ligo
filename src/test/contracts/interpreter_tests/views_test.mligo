module Test = Test.Next

#import "./contract_under_test/views_contract.mligo" "CUT"

let test =
  let _baker = Test.Account.address 0 in
  let _src = Test.Account.address 1 in
  let init_storage = 0 in
  let {taddr = addr_v ; code = _ ; size = _} =
    Test.Originate.contract
      (contract_of CUT.Main_with_view) init_storage 0mutez in
  let {taddr = addr_c ; code = _ ; size = _} =
    Test.Originate.contract
      (contract_of CUT.Caller) init_storage 0mutez in
  let tx =
    Test.Typed_address.transfer
      addr_c
      (Main (Test.Typed_address.to_address addr_v))
      1tez in
  match tx with
    Success _ ->
      let x = Test.Typed_address.get_storage addr_c in
      Assert.assert (x = 2)
  | _ -> failwith "transfer to view caller failed"
