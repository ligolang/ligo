
type param = int
type storage = int
type ret = operation list * storage

let main (_p, s : param * storage) : ret =
  [], s
