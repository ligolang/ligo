let rec f(n, m : nat * (nat,string) big_map) : (nat,string) big_map =
  match is_nat(n - 1) with
    | None -> m
    | Some i -> f(i, Big_map.add i "" m)

[@entry]
let main () (s : int) : operation list * int =
  let s2 = s + 42 in
  let s3 = s2 * s2 * 2 in
  let m : (nat, string) big_map = (Big_map.empty : (nat, string) big_map) in
  let _a = f(5n, m) in
  (([] : operation list), s2)
