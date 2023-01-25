type t is Foo of int
type r is record[ a : int ; b : t ; c : string ]

const record[ a ; b = (Foo (x)) ; c ] = 
      record[ a = 1 ; b = Foo (2) ; c = "hey" ]

function main (const _ : unit; const _ : int) : list(operation) * int
  is (nil, a + x + String.length (c))
