type foobar is
| Foo of int
| Bar of bool

const foo : foobar = Foo (42)

const bar : foobar = Bar (True)

function fb(const p : foobar) : int is
  block { skip } with (case p of
  | Foo (n) -> n
  | Bar (t) -> 42
  end)
