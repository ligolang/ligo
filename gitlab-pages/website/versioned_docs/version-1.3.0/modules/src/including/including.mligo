module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module NewEuro =
  struct
    include Euro
    let ten : t = 10n
  end