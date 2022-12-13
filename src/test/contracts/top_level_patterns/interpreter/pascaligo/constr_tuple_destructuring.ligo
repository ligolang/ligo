type t is Foo of int

function f (const _ : unit) is {
  Test.log ("Once");
} with (1, (Foo (2), "hey"))

const (a, ((Foo (x)), c)) = f (Unit)

const test = 
  assert ((a + x + String.length (c)) = 6)
