type storage = string * nat * string * nat * string

type parameter = int

type return = operation list * storage

[@entry]
let main (_ : parameter) (store : storage) : return = [], store

[@view]
let v (_ : int) (_ : storage) : tez = 1000000mutez
