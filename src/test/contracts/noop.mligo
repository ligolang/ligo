[@entry]
let main (_p : unit) (s : unit) : operation list * unit =
  let f (x : unit) : unit = x in
  let s2 : unit = f s in
  let s3 : unit = f s2 in
  let s = f s3 in
  (([] : operation list), s)
