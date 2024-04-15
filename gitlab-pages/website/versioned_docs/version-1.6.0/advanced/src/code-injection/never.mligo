type parameter =
  Increment of int
| Extend of never

type storage = int

[@entry]
let main (action : parameter) (store : storage) : operation list * storage =
  [],
  (match action with
     Increment n -> store + n
   | Extend k    -> [%Michelson ({| { NEVER } |} : never -> int)] k)