include Tezos_base.TzPervasives.List

let range n =
  let rec aux acc n =
    if n = 0
    then acc
    else aux ((n-1) :: acc) (n-1)
  in
  List.rev (aux [] n)

let find_map f lst =
  let rec aux = function
    | [] -> None
    | hd::tl -> (
      match f hd with
      | Some _ as s -> s
      | None -> aux tl
    )
  in
  aux lst

let find_index f lst =
  let rec aux n = function
  | [] -> raise (Failure "find_index")
  | hd :: _ when f hd -> n
  | _ :: tl -> aux (n + 1) tl in
  aux 0 lst

let find_full f lst =
  let rec aux n = function
  | [] -> raise (Failure "find_index")
  | hd :: _ when f hd -> (hd, n)
  | _ :: tl -> aux (n + 1) tl in
  aux 0 lst

let assoc_i x lst =
  let rec aux n = function
    | [] -> raise (Failure "List:assoc_i")
    | (x', y) :: _ when x = x' -> (y, n)
    | _ :: tl -> aux (n + 1) tl
  in
  aux 0 lst

let rec from n lst =
  if n = 0
  then lst
  else from (n - 1) (tl lst)

let until n lst =
  let rec aux acc n lst =
    if n = 0
    then acc
    else aux ((hd lst) :: acc) (n - 1) (tl lst)
  in
  rev (aux [] n lst)
