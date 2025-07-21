module Foo = struct
  let x : nat ticket =
    Option.value_with_error "no ticket" (Tezos.Ticket.create 42n 42n)
end

let x = Foo.x, Foo.x
