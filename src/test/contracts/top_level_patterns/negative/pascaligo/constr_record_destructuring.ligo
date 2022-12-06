type t is Foo of int | Bar
type r is record[ a : int ; b : t ; c : string ]

const record[ a ; b = (Foo (x)) ; c ] = record[ a = 1 ; b = Foo (2) ; c = "hey" ]

const test = 
  assert ((a + x + String.length (c)) = 6)
