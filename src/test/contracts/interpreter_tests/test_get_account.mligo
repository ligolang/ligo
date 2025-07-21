let test =
  let {addr; pk; sk} = Test.Account.info 0n in
  let () = Test.IO.log (addr, pk, sk) in
  let () = Test.IO.log (Test.Address.get_balance addr) in
  ()
