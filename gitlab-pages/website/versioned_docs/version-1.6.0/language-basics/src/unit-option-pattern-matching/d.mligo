let div (a, b : nat * nat) : nat option =
  if b = 0n then None else Some (a/b)