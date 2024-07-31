module Test = Test.Next

let test_accounts =
  let initial_balances : tez list = [] in
  let () = Test.State.reset 3n initial_balances in
  let admin_account = Test.Account.address(0n) in
  let user_account1 = Test.Account.address(1n) in
  let user_account2 = Test.Account.address(2n) in

  let () = Test.IO.log (Test.Address.get_balance admin_account) in
  // 3800000000000mutez
  let () = Test.IO.log (Test.Address.get_balance user_account1) in
  // 3800000000000mutez
  Test.IO.log (Test.Address.get_balance user_account2)
  // 3800000000000mutez