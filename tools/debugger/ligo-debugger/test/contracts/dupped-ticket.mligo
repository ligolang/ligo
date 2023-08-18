[@entry]
let main (p, s : nat ticket * int) : operation list * int =
  let kek = Tezos.get_now () in
  let ((_, (value, _)), _) = Tezos.read_ticket p in
  let ((_, (value2, _)), _) = Tezos.read_ticket p in
  let s2 = if value + value2 > 10n then s * 2 else s / 2 in
  (([] : operation list), s2)
