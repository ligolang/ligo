type storage = int

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
  ([], store + 1)