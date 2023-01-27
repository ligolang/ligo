let test =
  let f = (fun (_ : (unit * unit)) -> ()) in
  Test.originate_uncurried f () 0tez
