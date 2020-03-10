let x = 1 [@@inline]
let foo (a : int): int =
  (let test = 2 + a [@@inline] in test) [@@inline]
let y = 1 [@@inline][@@other]
let bar (b : int): int =
  let test = fun (z : int) -> 2 + b + z [@@inline][@@foo][@@bar]
  in test b
