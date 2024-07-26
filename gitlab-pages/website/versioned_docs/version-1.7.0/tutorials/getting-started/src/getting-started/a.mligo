type storage = int
type return_type = operation list * storage

[@entry] let add (n : int) (storage : storage) : return_type = [], storage + n
[@entry] let sub (n : int) (storage : storage) : return_type = [], storage - n