let content0 = "hello world"
let amount0 = 10n

module Test = Test.Next
module Tezos = Tezos.Next

module Ticket = Tezos.Ticket
let assert = Test.Assert.assert

let () =
    let ticket0 = Ticket.create content0 amount0
                  |> Option.value_with_error "First ticket was not created" in
    let (ticketer, (content1, amount1)), ticket0 = Ticket.read ticket0 in
    let () = assert (ticketer = Tezos.get_self_address ()) in
    let () = assert (content0 = content1) in
    let () = assert (amount0 = amount1) in
    let ticket1 = Ticket.create content0 amount0
                  |> Option.value_with_error "Second ticket was not created" in
    let ticket2 = Ticket.join (ticket0, ticket1)
                  |> Option.value_with_error "Tickets were not joined" in
    let (_, (_, amount2)), _ = Ticket.read ticket2 in
    assert (amount2 = amount0 + amount0)
