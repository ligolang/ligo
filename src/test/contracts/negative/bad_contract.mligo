type storage = int

type parameter = nat

[@entry]
let main (action : parameter) (store : storage) : storage = store + 1
