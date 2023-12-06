type storage = int
type result = operation list * storage

[@entry] [@no_mutation]
let sub (delta : int) (store : storage) : result =
  [], store - delta