module Foo = struct
  let x = 41
end

let x = 1

module TFoo = struct
  let x = x
  let y = Foo.x
end

module Foo = TFoo

let u = Foo.x + Foo.y

let x = u
