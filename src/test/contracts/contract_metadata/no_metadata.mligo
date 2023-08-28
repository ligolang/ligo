type param = int

type storage = int

type ret = operation list * storage

[@entry]
let main (_p : param) (s : storage) : ret = [], s
