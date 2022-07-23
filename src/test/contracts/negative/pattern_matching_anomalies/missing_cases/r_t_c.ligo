type p is One | Two | Three | Four
type r is ((string * int) * (p * nat))

type t is record [ a : int ; b : r ; c : nat ]

function s (const x : t) is 
  case x of [
  | record [ a = _ ; c = _ ; b = ((x, y), (One, z)) ] -> Unit
  | record [ a = _; b = ((x, y), (Two, z)) ; c = _ ] -> Unit
  ]