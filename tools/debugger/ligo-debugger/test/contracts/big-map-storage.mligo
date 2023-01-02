let main (_, s : unit * (int, int) big_map) : operation list * (int, int) big_map =
  let res = Big_map.add 1000 7 s in
  (([] : operation list), res)
