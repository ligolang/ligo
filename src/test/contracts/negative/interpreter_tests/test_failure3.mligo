let test =
  let ut = Test.reset_state 2n [1n;1n] in
  let x = Test.compile_value 1 in
  Test.originate "./dummy.mligo" "a" x 
