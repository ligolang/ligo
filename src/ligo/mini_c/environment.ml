open Trace
open Types

type element = environment_element

module Small = struct
  open Append_tree

  type t' = environment_small'
  type t = environment_small

  let not_in_env' ?source s t' =
    let title () = match source with
      | None -> "Not in environment"
      | Some source -> Format.asprintf "Not in environment' (%s)" source in
    let content () =
      Format.asprintf "Variable : %s, Environment' : %a"
        s PP.environment_small' t' in
    error title content

  let not_in_env ?source s t =
    let title () = match source with
      | None -> "Not in environment"
      | Some source -> Format.asprintf "Not in environment (%s)" source in
    let content () =
      Format.asprintf "Variable : %s, Environment : %a"
        s PP.environment_small t in
    error title content

  let has' s = exists' (fun ((x, _):element) -> x = s)
  let has s = function
    | Empty -> false
    | Full x -> has' s x

  let empty : t = empty

  let get_opt = assoc_opt

  let append s (e:t) = if has (fst s) e then e else append s e

  let of_list lst =
    let rec aux = function
      | [] -> Empty
      | hd :: tl -> append hd (aux tl)
    in
    aux @@ List.rev lst


  let rec to_list' (e:t') =
    match e with
    | Leaf x -> [x]
    | Node {a;b} -> (to_list' a) @ (to_list' b)

  let to_list (e:t) =
    match e with
    | Empty -> []
    | Full x -> to_list' x

  type bound = string list

  let rec get_path' = fun s env' ->
    match env' with
    | Leaf (n, v) when n = s -> ok ([], v)
    | Leaf _ -> fail @@ not_in_env' ~source:"get_path'" s env'
    | Node {a;b} ->
        match%bind bind_lr @@ Tezos_utils.Tuple.map2 (get_path' s) (a,b) with
        | `Left (lst, v) -> ok ((`Left :: lst), v)
        | `Right (lst, v) -> ok ((`Right :: lst), v)

  let get_path = fun s env ->
    match env with
    | Empty -> fail @@ not_in_env ~source:"get_path" s env
    | Full x -> get_path' s x
end

type t = environment

let empty : t = [Small.empty]
let extend t : t = Small.empty :: t
let restrict t : t = List.tl t
let of_small small : t = [small]

let rec get_opt : t -> string -> type_value option = fun t k ->
  match t with
  | [] -> None
  | hd :: tl -> (
      match Small.get_opt hd k with
      | None -> get_opt tl k
      | Some v -> Some v
    )

let rec has x : t -> bool = function
  | [] -> raise (Failure "Schema.Big.has")
  | [hd] -> Small.has x hd
  | hd :: tl -> Small.has x hd || has x tl
let add x : t -> t = function
  | [] -> raise (Failure "Schema.Big.add")
  | hd :: tl -> Small.append x hd :: tl

type path = [`Left | `Right] list
let pp_path : _ -> path -> unit =
  let open Format in
  let aux ppf lr = match lr with
    | `Left -> fprintf ppf "L"
    | `Right -> fprintf ppf "R"
  in
  PP_helpers.(list_sep aux (const " "))

let rec get_path : string -> environment -> ([`Left | `Right] list * type_value) result = fun s t ->
  match t with
  | [] -> simple_fail "Get path : empty big schema"
  | [ x ] -> Small.get_path s x
  | Empty :: tl -> get_path s tl
  | hd :: tl -> (
      match%bind bind_lr_lazy (Small.get_path s hd, (fun () -> get_path s tl)) with
      | `Left (lst, v) -> ok (`Left :: lst, v)
      | `Right (lst, v) -> ok (`Right :: lst, v)
    )
