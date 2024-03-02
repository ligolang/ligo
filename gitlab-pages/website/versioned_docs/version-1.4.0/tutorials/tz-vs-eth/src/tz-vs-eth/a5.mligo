type storage = int
type result = operation list * storage

let doMultiplyBy2 (store : storage) : int = store * 2

let doMultiplyBy4 (store : storage) : int = doMultiplyBy2 (doMultiplyBy2 store)

[@entry] let multiplyBy4 (_ : unit) (s : storage) : result = [], doMultiplyBy4 s
[@entry] let multiplyBy16 (_ : unit) (s : storage) : result = [], doMultiplyBy4 (doMultiplyBy4 s)