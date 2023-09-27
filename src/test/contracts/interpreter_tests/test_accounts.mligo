let cast_implicit_account c : implicit_address = Test.cast_address c

let test_new =
  let (_sk, pk) = Test.new_account () in
  let pkh = Crypto.hash_key pk in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in
  let to_ = Test.nth_bootstrap_account 0 in
  let _ = Test.transfer_to_contract_exn c () 123tez in
  let _ = Test.set_source a in
  let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
  Test.get_balance_of_address a

let test_add =
  let () = Test.reset_state 2n ([] : tez list) in
  let sk = "edsk3FhQ1djEDDCfqseyfbrpwkw5ogTDAaryXAdQGhk5Vpw6VGgo6v" in
  let pk = ("edpkv2kByfiJmUHr3SCp2rpASF2xSEhT248MSNEAZK9ho86sMBdcuE" : key) in
  let () = Test.add_account sk pk in
  let pkh = Crypto.hash_key pk in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in
  let to_ = Test.nth_bootstrap_account 0 in
  let _ = Test.transfer_to_contract_exn c () 123tez in
  let _ = Test.set_source a in
  let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
  Test.get_balance_of_address a
