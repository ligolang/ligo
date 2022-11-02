
(*
  In this case, the two records DON'T have the same field labels.

  TODO : Add a diff for records, just like tuples,
  to clarify where is the mismatch.
*)

type a = {foo : int ; bar : (nat * string) ; third_field : tez}
type b = {foo : int ; bar : (nat * nat   )}

let main (_p, s : int * int) : operation list * int =
  let y : a = {foo = 1 ; bar = (2n, "lol") ; third_field = 42tez } in
  let x : b = y in
  ([] : operation list), s
