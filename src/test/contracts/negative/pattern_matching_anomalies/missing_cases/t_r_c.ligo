type p is One | Two | Three | Four;
type r is record [ a : int ; b : p ; c : nat ]

type t is (int * (r * string))

function s (const x : t) is
  case x of [
  | (n, (record [ a = _ ; c = _ ; b = One ], s))  -> Unit
  | (n, (record [ a = _ ; b = Four ; c = _ ], s)) -> Unit
  ]