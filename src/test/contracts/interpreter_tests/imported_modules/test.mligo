#import "main.mligo" "Main"

module Test = Test.Next

let test1 =
  let _ = Test.Originate.contract (contract_of Main) "a" 1tez in
  ()
