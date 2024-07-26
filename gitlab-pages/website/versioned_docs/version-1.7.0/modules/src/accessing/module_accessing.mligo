module Euro =
  struct
    type t = int
    let add (a, b : t * t) : t = a + b
    let one : t = 1
    let two : t = 2
  end