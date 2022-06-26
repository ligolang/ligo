let nth_exn (type a) (i: int) (a: a list) : a =
  let rec aux (remaining: a list) (cur: int) : a =
    match remaining with
    [] ->
    failwith "Not found in list"
    | hd :: tl ->
      if cur = i then
      hd
      else aux tl (cur + 1)
  in
  aux a 0

