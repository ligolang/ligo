type foo is record [ a : int ; b : nat ; c : string ]

const yy : string = case (record [ a = 1 ; b = 2n ; c = "33" ]) of [
  | record [ a = a ;  b = b ; c = a ] -> a
]