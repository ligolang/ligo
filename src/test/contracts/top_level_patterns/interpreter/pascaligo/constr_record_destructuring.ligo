type t is Foo of int
type r is record[ a : int ; b : t ; c : string ]

function f (const _ : unit) is {
  Test.log ("Once");
} with record[ a = 1 ; b = Foo (2) ; c = "hey" ]
  
const record[ a ; b = (Foo (x)) ; c ] = f (Unit)

const test = 
  assert ((a + x + String.length (c)) = 6)
