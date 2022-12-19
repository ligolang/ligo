module Foo = struct
  let x : nat ticket = Option.unopt (Tezos.create_ticket 42n 42n)
end

let x = (Foo.x, Foo.x)

