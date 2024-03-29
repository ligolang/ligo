type 'storage return = operation list * 'storage

[@view]
let add (param : int) (storage : int) : int = param + storage

[@view]
let get_storage (_ : int) (storage : int) : int = storage

[@entry]
let main () (storage : int) : int return =
  ([] : operation list), storage
