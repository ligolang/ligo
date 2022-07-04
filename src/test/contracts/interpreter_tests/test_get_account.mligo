let test =
  let (a, pk, sk) = Test.get_bootstrap_account 0n in
  let () = Test.log (a, pk, sk) in
  let () = Test.log (Test.get_balance a) in
  ()
