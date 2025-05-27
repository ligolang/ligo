type storage = int
type result = operation list * storage

[@entry]
let increment (_ : unit) (s : storage) : result = [], s + 1

[@entry]
let decrement (_ : unit) (s : storage) : result = [], s - 1