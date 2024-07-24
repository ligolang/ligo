let content0 = "hello world"
let amount0 = 10n

let () = 
    let ticket0 = Tezos.create_ticket content0 amount0 |> Option.unopt in
    let ((ticketer, (content1, amount1)), ticket0) = Tezos.read_ticket ticket0 in
    let () = Test.assert (ticketer = Tezos.get_self_address ()) in 
    let () = Test.assert (content0 = content1) in
    let () = Test.assert (amount0 = amount1) in
    let ticket1 = Tezos.create_ticket content0 amount0 |> Option.unopt in
    let ticket2 = Tezos.join_tickets (ticket0, ticket1) |> Option.unopt in 
    let ((_, (_, amount2)), _) = Tezos.read_ticket ticket2 in
    Test.assert (amount2 = amount0 + amount0) 