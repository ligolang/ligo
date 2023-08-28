type storage = (int, int) map

type parameter = int list

[@entry]
let main (_p : parameter) (s : storage) : operation list * storage = ([], s)
