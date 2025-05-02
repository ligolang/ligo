type t = Foo of int | Bar
type r = { a : int ; b : t ; c : string }

let { a ; b = (Foo x) ; c} = { a = 1 ; b = Foo 2 ; c = "hey" }

let test =  Assert.assert (a + x + String.length c = 6)
