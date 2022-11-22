type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 (
    (nil : list (operation)),
    case action of [
      Increment (n) -> store(n)
    | Decrement (n) -> store(n)
    | Reset         -> 0
    ]
  )
