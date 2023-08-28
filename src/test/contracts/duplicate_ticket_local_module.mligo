let fst (x, _) = x

let snd (_, x) = x

[@entry]
let main () (_ : nat ticket) : operation list * nat ticket =
  module B = struct
    let ticket = Option.unopt (Tezos.create_ticket 10n 10n)

    let y = ticket, ticket

  end in
  [], Option.unopt (Tezos.join_tickets (fst B.y, snd B.y))
