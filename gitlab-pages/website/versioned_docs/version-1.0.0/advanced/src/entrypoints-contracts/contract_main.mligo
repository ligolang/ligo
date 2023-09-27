type parameter =
  Action_A of nat
| Action_B of string

type storage = {
  counter : nat;
  name    : string
}

type return = operation list * storage

let entry_A (n : nat) (store : storage) : return =
  [], {store with counter = n}

let entry_B (s : string) (store : storage) : return =
  [], {store with name = s}

let main (action : parameter) (store: storage) : return =
  match action with
    Action_A n -> entry_A n store
  | Action_B s -> entry_B s store
