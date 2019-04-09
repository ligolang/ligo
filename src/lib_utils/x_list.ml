include Tezos_base.TzPervasives.List

let filter_map f =
  let rec aux acc lst = match lst with
    | [] -> List.rev acc
    | hd :: tl -> aux (
        match f hd with
        | Some x -> x :: acc
        | None -> acc
      ) tl
  in
  aux []

let cons_iter = fun fhd ftl lst ->
  match lst with
  | [] -> ()
  | hd :: tl -> fhd hd ; List.iter ftl tl

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

module Ne = struct

  type 'a t = 'a * 'a list

  let of_list lst = List.(hd lst, tl lst)
  let to_list (hd, tl : _ t) = hd :: tl
  let iter f (hd, tl : _ t) = f hd ; List.iter f tl
  let map f (hd, tl : _ t) = f hd, List.map f tl
  let mapi f (hd, tl : _ t) =
    let lst = List.mapi f (hd::tl) in
    of_list lst
  let concat (hd, tl : _ t) = hd @ List.concat tl
  let rev (hd, tl : _ t) =
    match tl with
    | [] -> (hd, [])
    | lst ->
        let r = List.rev lst in
        (List.hd r, List.tl r @ [hd])

end
