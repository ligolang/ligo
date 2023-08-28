type action =
| Add of int
| Sub of int
| Reset

type storage = int

module Math = struct
  let add (a : int) (b : int) : int = a + b

  let sub (a : int) (b : int) : int = a - b

  let zero = 0

end

[@entry]
let main (action : action) (storage : storage) : operation list * storage =
  match action with
    Add x -> ([] : operation list), Math.add storage x
  | Sub x -> ([] : operation list), Math.sub storage x
  | Reset -> ([] : operation list), Math.zero
