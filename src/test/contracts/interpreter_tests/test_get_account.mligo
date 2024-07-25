let test =
  let {addr; pk; sk} = Test.Next.Account.info 0n in
  let () = Test.Next.IO.log (addr, pk, sk) in
  let () = Test.Next.IO.log (Test.Next.Address.get_balance addr) in
  ()
