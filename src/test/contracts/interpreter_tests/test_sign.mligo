let test =
  let (sk, pk) = Test.new_account () in
  let data = Bytes.pack "Bonjour le monde !" in
  let s = Test.sign sk data in
  assert (Crypto.check pk s data)
