module Tezos = Tezos.Next

module Foo = struct
  let x : nat ticket = Option.unopt (Tezos.Ticket.create 42n 42n)
end

let x = Foo.x, Foo.x
