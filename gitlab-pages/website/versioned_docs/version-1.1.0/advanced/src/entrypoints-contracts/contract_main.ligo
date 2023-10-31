type parameter is
  Action_A of nat
| Action_B of string

type storage is
  record [
    counter : nat;
    name    : string
  ]

type return is list (operation) * storage

function entry_A (const n : nat; const store : storage) : return is
  (nil, store with record [counter = n])

function entry_B (const s : string; const store : storage) : return is
  (nil, store with record [name = s])

function main (const action : parameter; const store : storage): return is
  case action of [
    Action_A (n) -> entry_A (n, store)
  | Action_B (s) -> entry_B (s, store)
  ]
