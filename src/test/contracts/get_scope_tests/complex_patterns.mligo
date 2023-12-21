let a, b = 3, "hello"

type t = { a : int; b : string }
let { a; b } = { a; b }

type u = Foo of int
let (Foo x) = Foo 42

type v = Bar of int * string
let (Bar (i, j)) = Bar (a, b)

type w = Qux of t
let (Qux {a; b}) = Qux {a; b}
