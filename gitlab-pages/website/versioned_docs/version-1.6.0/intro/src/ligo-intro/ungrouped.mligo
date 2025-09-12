  type storage = string

  [@entry]
  let store_hello (delta : int) (store : storage) : operation list * storage = [], "Hello"