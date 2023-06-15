module Foo = struct
  let module_field = 5
  let another_thing = "asdad"

  module Bar = struct
    let nested = { left = 1; right = 2 }
  end
end

let x = Foo.
let y = Foo.Bar.
let z = Foo.Bar.nested.
