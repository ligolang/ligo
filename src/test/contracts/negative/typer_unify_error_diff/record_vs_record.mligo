
(*
  In this case,
  the two records have the same field labels,
  but with a type mismatch in one of their fields,
  the typer will pinpoint the precise type mismatch :
  here, [string] vs. [nat]

  TODO : We should add location to show where is the
  [string] and [nat] in the source code,
  otherwise it can be difficult to see where is
  the mismatch when the types are long.
*)

type a = {foo : int ; bar : (nat * string)}
type b = {foo : int ; bar : (nat * nat   )}

let main (_p, s : int * int) : operation list * int =
  let y : a = {foo = 1 ; bar = (2n, "lol") } in
  let x : b = y in
  ([] : operation list), s
