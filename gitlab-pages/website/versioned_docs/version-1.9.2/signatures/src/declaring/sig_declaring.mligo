module type Euro_SIG =
  sig
    type t
    val add : t * t -> t
    val one : t
    val two : t
  end
module Euro : Euro_SIG =
  struct
    type t = nat // No more abstract
    let add (a, b : t * t) = a + b
    let one : t = 1n
    let two : t = 2n
  end