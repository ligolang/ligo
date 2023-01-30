let test =
  let f = (fun (_ : unit) (_ : unit) -> ()) in
  Test.originate f () 0tez
