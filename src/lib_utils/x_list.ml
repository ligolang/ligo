include Tezos_base.TzPervasives.List

let map ?(acc = []) f lst =
  let rec aux acc f = function
    | [] -> acc
    | hd :: tl -> aux (f hd :: acc) f tl
  in
  aux acc f (List.rev lst)

let fold_map_right : type acc ele ret . (acc -> ele -> (acc * ret)) -> acc -> ele list -> ret list =
  fun f acc lst ->
  let rec aux (acc , prev) f = function
    | [] -> (acc , prev)
    | hd :: tl ->
        let (acc' , hd') = f acc hd in
        aux (acc' , hd' :: prev) f tl
  in
  snd @@ aux (acc , []) f (List.rev lst)

let fold_map : type acc ele ret . (acc -> ele -> (acc * ret)) -> acc -> ele list -> ret list =
  fun f acc lst ->
  let rec aux (acc , prev) f = function
    | [] -> (acc , prev)
    | hd :: tl ->
        let (acc' , hd') = f acc hd in
        aux (acc' , hd' :: prev) f tl
  in
  List.rev @@ snd @@ aux (acc , []) f lst

let fold_right' f init lst = List.fold_left f init (List.rev lst)

let rec remove_element x lst =
  match lst with
  | [] -> raise (Failure "X_list.remove_element")
  | hd :: tl when x = hd -> tl
  | hd :: tl -> hd :: remove_element x tl

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
  aux [] n

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

let uncons_opt = function
  | [] -> None
  | hd :: tl -> Some (hd, tl)

let rev_uncons_opt = function
  | [] -> None
  | lst ->
      let r = rev lst in
      let last = hd r in
      let hds = rev @@ tl r in
      Some (hds, last)

let to_pair = function
  | [a ; b] -> Some (a , b)
  | _ -> None

let to_singleton = function
  | [a] -> Some a
  | _ -> None

module Ne = struct

  type 'a t = 'a * 'a list

  let of_list lst = List.(hd lst, tl lst)
  let to_list (hd, tl : _ t) = hd :: tl
  let singleton hd : 'a t = hd , []
  let hd : 'a t -> 'a = fst
  let cons : 'a -> 'a t -> 'a t = fun hd' (hd , tl) -> hd' , hd :: tl
  let iter f (hd, tl : _ t) = f hd ; List.iter f tl
  let map f (hd, tl : _ t) = f hd, List.map f tl
  let hd_map : _ -> 'a t -> 'a t = fun f (hd , tl) -> (f hd , tl)
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
  let find_map = fun f (hd , tl : _ t) ->
    match f hd with
    | Some x -> Some x
    | None -> find_map f tl

end
