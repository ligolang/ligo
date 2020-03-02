let sum (p : int * int) : int =
  let i, result = p in i + result

let sum2 (p : string * string * string * string) : int =
  let a, b, c, d = p in a ^ b ^ c ^ d
