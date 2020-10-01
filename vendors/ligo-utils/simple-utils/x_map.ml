module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val of_list : (key * 'a) list -> 'a t
  val to_list_rev : 'a t -> 'a list
  val to_kv_list_rev : 'a t -> (key * 'a) list
  val to_list : 'a t -> 'a list
  val to_kv_list : 'a t -> (key * 'a) list
  val add_bindings : (key * 'a) list -> 'a t -> 'a t
  val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
end

module Make(Ord : Map.OrderedType) : S with type key = Ord.t = struct
  include Map.Make(Ord)

  let of_list (lst: (key * 'a) list) : 'a t =
    let aux prev (k, v) = add k v prev in
    List.fold_left aux empty lst

  let to_list_rev (t: 'a t) : 'a list =
    let aux _k v prev = v :: prev in
    fold aux t []

  let to_kv_list_rev (t: 'a t) : (key * 'a) list =
    let aux k v prev = (k, v) :: prev in
    fold aux t []

  let to_list l = List.rev @@ to_list_rev l
  let to_kv_list l = List.rev @@ to_kv_list_rev l

  let add_bindings (kvl:(key * 'a) list) (m:'a t) =
    let aux prev (k, v) = add k v prev in
    List.fold_left aux m kvl

  let fold_map f acc map =
    let aux k v (acc,map) =
      let acc,v = f acc k v in
      acc, add k v map
    in
    fold aux map (acc,empty)
end

module String = Make(String)
