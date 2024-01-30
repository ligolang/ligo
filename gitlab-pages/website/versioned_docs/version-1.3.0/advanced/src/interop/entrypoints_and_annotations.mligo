type storage = int

[@entry] let left (i : int) (x : storage) : operation list * storage = [], x - i
[@entry] let right (i : int) (x : storage) : operation list * storage = [], x + i