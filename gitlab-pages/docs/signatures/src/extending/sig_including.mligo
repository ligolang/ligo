module type Euro_SIG =
  sig
    type t
    val add : t * t -> t
    val one : t
    val two : t
  end

module type NewEuro_SIG =
  sig
    include Euro_SIG
    val ten : t
  end