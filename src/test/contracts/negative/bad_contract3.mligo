type storage = int

type parameter = nat

type return = operation list * string

[@entry]
let main (action : parameter) (store : storage) : return =
  (([] : operation list), "bad")
