let is_it_a_nat (i : int) =
  match is_nat i with
    None   -> false
  | Some _ -> true