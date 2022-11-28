type t is Foo of int

const (a, ((Foo (x)), y)) = (1, (Foo (2), "hey"))

function main (const _ : unit; const _ : int) : list(operation) * int
  is (nil, a + x + String.length (y))