module Foo = struct
  [@entry]
  let b () () : operation list * unit = failwith ()
  [@entry]
  let b () () : operation list * unit  = failwith ()
end
