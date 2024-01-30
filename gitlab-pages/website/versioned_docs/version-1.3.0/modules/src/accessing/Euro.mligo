module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

type storage = Euro.t

let tip (s : storage) : storage =
  Euro.add (s, Euro.one)