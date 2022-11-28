type t is Foo of int | Bar

const (a, (Foo (x)), c) = (1, Foo (2), "hey")

const test = 
  assert ((a + x + String.length (c)) = 6)
