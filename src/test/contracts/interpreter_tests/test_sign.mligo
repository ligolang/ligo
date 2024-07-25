module Test = Test.Next
let assert = Assert.assert

let test =
  let {addr=_; sk; pk} = Test.Account.new () in
  let data = Bytes.pack "Bonjour le monde !" in
  let s = Test.Crypto.sign sk data in
  assert (Crypto.check pk s data)
