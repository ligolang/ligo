module Foo = struct
  let x = "hehe"
  let foo (z : int * unit) : int = match z with (x, _u) -> x
end

let test = Foo.foo (42, unit)
