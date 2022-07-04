(*
  Here, we expect no warning on any ambiguous sum type
  because the types in the shadowed module are inaccessible.
*)

module Mod_a = struct
  type tshadowed =
    | TopS of int
    | AS of int
  let dummy = 0

  module Mod_nested = struct
    type tn =
      | TopN of int
      | AN of int
    let dummy = 0
  end
end

module Mod_a = struct
  type ta =
    | AS of int
    | AN of int
end

type ttop =
  | TopS of int
  | TopN of int

let dont_warn_me = TopS 42
let dont_warn_me = AS 42
let dont_warn_me = TopN 42
let dont_warn_me = AN 42

let main (_: int * int) : operation list * int = ([] : operation list), 42
