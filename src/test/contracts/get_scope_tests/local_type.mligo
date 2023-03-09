let local_type (u : unit) : int =
  let y = 42 in
  type toto = int in
  let foo (b : toto) : toto = b in
  titi

let x = 42
