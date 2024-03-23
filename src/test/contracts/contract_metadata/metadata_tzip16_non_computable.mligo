[@tzip16_compatible]
let infinite_loop =
  let rec f (x : int) : int = f (x + 1) in
  f 0

[@tzip16_compatible]
let failing = failwith "My err"
