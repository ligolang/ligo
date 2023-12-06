type storage = int
type result = operation list * storage

[@entry]
let add (i : int) (s : storage) : result = [], s + i

[@entry]
let subtract (i : int) (s : storage) : result = [], s - i