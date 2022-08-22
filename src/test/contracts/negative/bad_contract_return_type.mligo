type paramater = unit
type storage = int
type _return = operation list * storage * tez

let main (_,s : paramater * storage) : _return =
    [], s, 1tez