module Test = Test.Next

let x = 2
let x = 1

let f (y : int) =
  y * x

let test =
  let v = Test.Michelson.run f 4 in
  Test.IO.log v
