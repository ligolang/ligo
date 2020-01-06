include List

let rec remove n = function
  | [] -> raise (Failure "List.remove")
  | _  :: tl when n = 0 -> tl
  | hd :: tl -> hd :: remove (n - 1) tl

let rec remove_from n = function
  | [] -> raise (Failure "List.remove_from")
  | _ when n = 0 -> []
  | hd :: tl -> hd :: remove_from (n - 1) tl

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

let fold_map_acc : type acc ele ret . (acc -> ele -> (acc * ret)) -> acc -> ele list -> acc * ret list =
  fun f acc lst ->
  let rec aux (acc , prev) f = function
    | [] -> (acc , prev)
    | hd :: tl ->
        let (acc' , hd') = f acc hd in
        aux (acc' , hd' :: prev) f tl
  in
  let (acc, lst) = aux (acc , []) f lst in
  (acc, List.rev lst)

let fold_map : type acc ele ret . (acc -> ele -> (acc * ret)) -> acc -> ele list -> ret list =
  fun f acc lst ->
  snd (fold_map_acc f acc lst)

let fold_right' f init lst = List.fold_left f init (List.rev lst)

(* breaking a dependency cycle with X_option *)
let unopt ~default x = match x with
  | None -> default
  | Some x -> x

let rec remove_element ?compare:cmp x lst =
  let compare = unopt ~default:compare cmp in
  match lst with
  | [] -> raise (Failure "X_list.remove_element")
  | hd :: tl when compare x hd = 0 -> tl
  | hd :: tl -> hd :: remove_element ~compare x tl

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

let assoc_i ~compare x lst =
  let rec aux n = function
    | [] -> raise (Failure "List:assoc_i")
    | (x', y) :: _ when compare x x' = 0 -> (y, n)
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
      Some (hds , last)

let hds lst = match rev_uncons_opt lst with
  | None -> failwith "toto"
  | Some (hds , _) -> hds

let to_pair = function
  | [a ; b] -> Some (a , b)
  | _ -> None

let to_singleton = function
  | [a] -> Some a
  | _ -> None


(** overriding stdlib List functions with optional compare/eq
   arguments *)

let rec mem ?compare:cmp x =
  let compare = unopt ~default:compare cmp in
  function
  | [] -> false
  | a::l -> compare a x = 0 || mem ~compare x l

let rec memq ?eq:eq x =
  let eq = unopt ~default:(=) eq in
  function
  | [] -> false
  | a::l -> eq a x || memq ~eq x l

let rec assoc ?compare:cmp x =
  let compare = unopt ~default:compare cmp in
  function
    [] -> raise Not_found
  | (a,b)::l -> if compare a x = 0 then b else assoc ~compare x l

let rec assoc_opt ?compare:cmp x =
  let compare = unopt ~default:compare cmp in
  function
    [] -> None
  | (a,b)::l -> if compare a x = 0 then Some b else assoc_opt ~compare x l


module Ne = struct

  type 'a t = 'a * 'a list

  let of_list lst = List.hd lst, List.tl lst
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
