type storage = int

[@entry]
let sub (i : int) (x : storage) : operation list * storage = [], x - i

[@entry]
let add (i : int) (x : storage) : operation list * storage = [], x + i