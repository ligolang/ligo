module EURO =
  struct
    type t = int
    let add (a, b : t * t) : t = a + b
    let zero : t = 0
    let one : t = 1
  end