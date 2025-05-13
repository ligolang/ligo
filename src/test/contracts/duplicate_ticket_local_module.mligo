let fst (x, _) = x
let snd (_, x) = x

[@entry]
let main () (_ : nat ticket) : operation list * nat ticket =
  module B = struct
    let ticket = Option.value_with_error "option is None" (Tezos.Ticket.create 10n 10n)
    let y = ticket, ticket
  end in
  [], Option.value_with_error "option is None" (Tezos.Ticket.join (fst B.y, snd B.y))
