module D = struct
  type t = bool
  type p = { initial : t; final : t }
  let default : p = { initial = true; final = false }
end

[@entry]
let main (p : D.t) (_ : D.t) : operation list * D.t =
  ([] : operation list), p

