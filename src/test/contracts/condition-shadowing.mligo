(* TODO : make a test using mutation, not shadowing *)
let%entry main (i : int) =
  let result = 0 in
  if i = 2 then
    let result = 42 in
    result
  else
    let result = 0 in
    result
