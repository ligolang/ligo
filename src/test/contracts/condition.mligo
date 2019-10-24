// Test if conditional in CameLIGO

let main (i : int) : int =
  let result : int = 23 in
  if i = 2 then 42 else 0

let foo (b : bool) : int =
  let x : int = 41 in
  let x : int = 1 + (if b then x else main(x)) in x
