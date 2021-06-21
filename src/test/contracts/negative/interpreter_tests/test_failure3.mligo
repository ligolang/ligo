let test =
  let ut = Test.reset_state 2n [1n;1n] in
  let f = (fun (_ : (unit * unit)) -> ()) in
  Test.originate f () 0tez
