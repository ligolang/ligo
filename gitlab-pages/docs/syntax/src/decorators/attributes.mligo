type storage = int
type result = operation list * storage

[@entry] [@no_mutation]
let sub (delta : int) (storage : storage) : result =
  [], storage - delta