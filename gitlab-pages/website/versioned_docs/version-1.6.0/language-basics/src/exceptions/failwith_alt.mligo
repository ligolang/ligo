[@entry]
let main (p : bool) (s : unit) : operation list * unit =
  let u : unit = assert p
  in [], s

[@entry]
let some (o : unit option) (s : unit) : operation list * unit =
  let u : unit = assert_some o
  in [], s