type paramater = unit
type storage = int
type _return = operation list * storage * tez

let main (_ : parameter) (s : storage) : _return =
    [], s, 1tez