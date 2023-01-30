type storage = string * nat * string * nat * string

type parameter = int
type return = operation list * storage

let main (_ : parameter) (store : storage) : return =
 [], store

[@view] let v (_ : int) (_ : storage) : tez = 1tez