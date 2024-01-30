
[@inline]
let fst (p : nat * nat) = p.0

[@entry]
let main (p : nat * nat) (s : nat * nat) : operation list * (nat * nat) =
    ([], (fst (p.0, p.1), fst (s.1, s.0)))