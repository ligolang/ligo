let test_accounts =
  let initial_balances : tez list = [] in
  let () = Test.Next.State.reset 3n initial_balances in
  let admin_account = Test.Next.Account.address(0n) in
  let user_account1 = Test.Next.Account.address(1n) in
  let user_account2 = Test.Next.Account.address(2n) in

  let () = Test.Next.IO.log (Test.Next.Address.get_balance admin_account) in
  // 3800000000000mutez
  let () = Test.Next.IO.log (Test.Next.Address.get_balance user_account1) in
  // 3800000000000mutez
  Test.Next.IO.log (Test.Next.Address.get_balance user_account2)
  // 3800000000000mutez