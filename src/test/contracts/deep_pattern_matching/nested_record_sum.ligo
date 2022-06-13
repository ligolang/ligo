type a is A | B of int | C of int * int * int | D | E ;
type r is record [ a : int; b : a ] ;

function f (const a : r) : int is
  case a of [
  | record [ a = _    ; b = A        ] -> 1
  | record [ b = B(_) ; a = _        ] -> 2
  | record [ a = _    ; b = C(_,_,_) ] -> 3
  | record [ b = D    ; a = _        ] -> 4
  | record [ a = _    ; b = E        ] -> 5
  ]

function main (const _ : unit*int):(list(operation) * int) is
  {
    const x = f (record [ a = 1 ; b = A ]);
  } with (nil, x)