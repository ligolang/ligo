module Test = Test.Next

let test =
  let x = Test.Michelson.eval 4n in
  (Test.Michelson.decompile x : string)
