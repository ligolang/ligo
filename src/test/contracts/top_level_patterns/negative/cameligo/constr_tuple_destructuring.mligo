type t = Foo of int | Bar

let (a,  (Foo x), c) = (1, Foo 2, "hey")

let test = 
  assert ((a + x + String.length c) = 6)
