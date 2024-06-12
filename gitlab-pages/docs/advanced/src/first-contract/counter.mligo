type storage = int
type result = operation list * storage

[@entry] let add (n : int) (store : storage) : result = [], store + n
[@entry] let sub (n : int) (store : storage) : result = [], store - n

[@view] let get_value (_ : unit) (store : storage) : int = store