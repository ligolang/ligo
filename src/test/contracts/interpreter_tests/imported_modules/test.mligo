#import "main.mligo" "Main"

let test1 =
  let _ = Test.originate (contract_of Main) "a" 1tez in
  ()