let a = 1

let b =
  let rec c : int*int -> int = fun ((i,j):int*int) ->
    let k = i + j + a in
    c (k,1)
  in
  let rec z (n : int) (m : int) : int = 
    if n = 0 then m else z (n - 1) m
  in
  let v = z 10 999 in
  let b = 2 + v in
  c (a, b)

let rec x (y : int) : unit =
  if y = 0 then ()
  else x (y - 1)