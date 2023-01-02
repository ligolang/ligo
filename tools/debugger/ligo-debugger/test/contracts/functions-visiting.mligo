let f(a: int): int =
    let x = a + 100
    in let x2 = x + 100
    in x2

let g(): int =
    let y = 10 + 10
    in y

let main (_, s : unit * int) : operation list * int =
  let s2 = s + 1 in
  let s3 = g() + f(s2) in
  (([] : operation list), s3)
