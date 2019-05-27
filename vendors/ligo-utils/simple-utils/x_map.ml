module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> 'a list
  val to_kv_list : 'a t -> (key * 'a) list
end

module Make(Ord : Map.OrderedType) : S with type key = Ord.t = struct
  include Map.Make(Ord)

  let of_list (lst: (key * 'a) list) : 'a t =
    let aux prev (k, v) = add k v prev in
    List.fold_left aux empty lst

  let to_list (t: 'a t) : 'a list =
    let aux _k v prev = v :: prev in
    fold aux t []

  let to_kv_list (t: 'a t) : (key * 'a) list =
    let aux k v prev = (k, v) :: prev in
    fold aux t []
end

module String = Make(String)
