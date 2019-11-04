let main (p: bool) (s: unit) =
  let u : unit = assert p
  in ([] : operation list), s
