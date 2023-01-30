(*
  We expect an error because constructor [A] is inaccessible
  since it is in a shadowed module
*)
module Mod_a = struct
  type tshadowed = A of int
end

module Mod_a = struct
  let dummy = 0
end

let x = A 42

let main (_: int) (_ : int) : operation list * int = ([] : operation list), 42

