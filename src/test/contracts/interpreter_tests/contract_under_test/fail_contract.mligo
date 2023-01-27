type storage = unit
type return = operation list * storage
type parameter = unit

let fail_data = "my contract always fail"

let main (_action : parameter) (_store : storage) : return =
  (([] : operation list), failwith fail_data)
