type v = Foo of int * string
let (Foo (i, j)) = let a = 42 in let b = "hello" in Foo (a, b)
