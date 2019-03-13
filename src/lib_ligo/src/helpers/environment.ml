module type PARAMETER = sig
  type key
  type value
  val key_cmp : key -> key -> int
  val value_cmp : value -> value -> int
end

let parameter (type key value) ?key_cmp ?value_cmp () : (module PARAMETER with type key = key and type value = value) =
  (module struct
    type nonrec key = key
    type nonrec value = value
    let key_cmp = Option.unopt ~default:compare key_cmp
    let value_cmp = Option.unopt ~default:compare value_cmp
  end)

let int_parameter = (parameter () : (module PARAMETER with type key = int and type value = int))
module INT_PARAMETER = (val ((parameter () : (module PARAMETER with type key = int and type value = int))))

module type ENVIRONMENT = sig
  type key
  type value
  type t

  val empty : t
  val get_opt : t -> key -> value option
  val gets : t -> key -> value list
  val set : t -> key -> value -> t
  val del : t -> key -> t
end

module Make(P:PARAMETER) : ENVIRONMENT with type key = P.key and type value = P.value  = struct
  type key = P.key
  type value = P.value
  type t = (key * value) list

  let empty : t = []

  let gets lst k =
    let kvs = List.filter (fun (k', _) -> P.key_cmp k k' = 0) lst in
    List.map snd kvs
  let get_opt lst k = match gets lst k with
    | [] -> None
    | v :: _ -> Some v

  let set lst k v = (k, v) :: lst

  let del lst k =
    let rec aux acc = function
      | [] -> List.rev acc
      | (key, _) :: tl when P.key_cmp key k = 0 -> List.rev acc @ tl
      | hd :: tl -> aux (hd :: acc) tl in
    aux [] lst
end
