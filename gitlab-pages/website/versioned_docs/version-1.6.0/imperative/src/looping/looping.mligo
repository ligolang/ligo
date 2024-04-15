let rec iter (x, y : nat * nat) : nat =
  if y = 0n then x else iter (y, x mod y)

let gcd (x, y : nat * nat) : nat =
  iter (if x < y then y,x else x,y)
let gcd (a, b : nat * nat) =
  let mut x, y = a, b in // We will modify x and y
  let () =
    if x < y then
      let z = x in
      begin
        x := y; y := z // Swapping x and y
      end in
  let mut r : nat = 0n in
  let () =
    while y <> 0n do
      r := x mod y;
      x := y;
      y := r
    done
  in x
let get_char s idx = String.sub idx 1n s

let is_palindrome s =
  let mut p = "" in
  let length = String.length s in
  let () =
    for i = length - 1 downto 0 do
      p := p ^ get_char s (abs i)
    done
  in p = s