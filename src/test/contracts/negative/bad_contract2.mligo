type storage = int
type parameter = nat
type return = string * storage

let main (action : parameter) (store : storage) : return =
  ("bad",store + 1)