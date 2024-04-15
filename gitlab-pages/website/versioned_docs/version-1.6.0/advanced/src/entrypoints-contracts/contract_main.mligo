type parameter =
  Action_A of nat
| Action_B of string

type storage = {
  counter : nat;
  name    : string
}

type result = operation list * storage

let entry_A (n : nat) (store : storage) : result =
  [], {store with counter = n}

let entry_B (s : string) (store : storage) : result =
  [], {store with name = s}

[@entry]
let main (action : parameter) (store: storage) : result =
  match action with
    Action_A n -> entry_A n store
  | Action_B s -> entry_B s store