type storage = int
type return_type = operation list * storage

[@view]
let add (param : int) (storage : storage) : int = param + storage

[@view]
let get_storage (_ : int) (storage : storage) : int = storage

[@entry]
let main () (storage : storage) : return_type =
  ([] : operation list), storage