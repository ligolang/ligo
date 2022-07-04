let test =
  let x = Test.eval 4n in
  (Test.decompile x : string)
