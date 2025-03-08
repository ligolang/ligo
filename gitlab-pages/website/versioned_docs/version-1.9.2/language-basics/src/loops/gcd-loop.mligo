let gcd (a, b : nat * nat) =
  let mut x, y = a, b in // we will modify x and y
  let () =
    if x < y then
      let z = x in
      begin
        x := y; y := z
      end in
  let mut r : nat = 0n in
  let () =
    while y <> 0n do
      r := x mod y;
      x := y;
      y := r
    done
  in x