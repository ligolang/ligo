let fill (type a) (n : int) (x : a) : a list =
  let rec aux (n, acc : int * a list) : a list =
    if (n = 0) then acc
    else aux (n - 1, x :: acc) in
  aux (n, ([] : a list))

let rec last_exn (type a) (xs : a list) : a =
  match xs with
    [] -> failwith "Empty list"
  | [hd] -> hd
  | _ :: tl -> last_exn tl

let reverse (type a) (xs : a list) : a list =
  let f (ys, x : (a list * a)) : a list = x :: ys in
  List.fold_left f ([] : a list) xs

let concat (type a) (xs : a list) (ys : a list) : a list =
  let f (x, ys : (a * a list)) : a list = x :: ys in
  List.fold_right f xs ys

let get_exn (type a) (xs : a list) (n : int) : a =
  let rec aux (remaining : a list) (cur : int) : a =
    match remaining with
      [] -> failwith "Not found in list"
    | hd :: tl -> if cur = n then hd else aux tl (cur + 1) in
  aux xs 0

let set_exn (type a) (xs : a list) (n : int) (x : a) : a list =
  let rec aux (xs, n, acc : a list * int * a list)
  : a list =
    match xs with
      [] -> failwith "Not found in list"
    | hd :: tl ->
        if (n = 0)
        then
          let ys = reverse (x :: acc) in
          concat ys tl
        else aux (tl, n - 1, hd :: acc) in
  aux (xs, n, ([] : a list))

let insert_exn (type a) (xs : a list) (n : int) (x : a) : a list =
  let rec aux (xs, n, acc : a list * int * a list)
  : a list =
    match xs with
      [] -> failwith "Not found in list"
    | hd :: tl ->
        if (n = 0)
        then
          let ys = reverse (x :: acc) in
          concat ys xs
        else aux (tl, n - 1, hd :: acc) in
  aux (xs, n, ([] : a list))

let remove_exn (type a) (xs : a list) (n : int) : a list =
  let rec aux (xs, n, acc : a list * int * a list) : a list =
    match xs with
      [] -> failwith "Not found in list"
      | hd :: tl ->
        if (n = 0)
        then
          let ys = reverse acc in
          concat ys tl
        else aux (tl, n - 1, hd :: acc) in
  aux (xs, n, ([] : a list))

let rec drop_exn (type a) (xs : a list) (n : int) : a list =
  match xs with
    [] -> failwith "Not found in list"
  | _hd :: tl -> if (n = 0) then xs else drop_exn tl (n - 1)

let take (type a) (xs : a list) (n : int) : a list =
  let rec aux (xs, n, acc : a list * int * a list) : a list =
    if (n = 0)
    then reverse acc
    else
      match xs with
        [] -> reverse acc
      | hd :: tl -> aux (tl, n - 1, hd :: acc) in
  aux (xs, n, ([] : a list))

let slice (type a) (xs : a list) (start : int) (range : int)
: a list =
  take (drop_exn xs start) range

let split (type a) (xs : a list) (n : int) : a list * a list =
  take xs n, drop_exn xs n

let rotate (type a) (xs : a list) (n : int) : a list =
  let rec aux (xs, n, acc : a list * int * a list) : a list =
    if (n = 0)
    then
      let ys = reverse acc in
      concat xs ys
    else
      match xs with
        [] -> reverse acc
      | hd :: tl -> aux (tl, n - 1, hd :: acc) in
  aux (xs, n, ([] : a list))
