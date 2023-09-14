module type OrderedType = Caml.Map.OrderedType

module type OrderedHashableType = sig
  include Caml.Map.OrderedType

  val hash_fold_t : Hash.state -> t -> Hash.state
end

module type S = sig
  include Caml.Map.S

  val diff : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val to_list_rev : 'a t -> 'a list
  val to_kv_list_rev : 'a t -> (key * 'a) list
  val to_list : 'a t -> 'a list
  val to_kv_list : 'a t -> (key * 'a) list
  val unzip : ('a * 'b) t -> 'a t * 'b t
  val keys : 'a t -> key list
  val values : 'a t -> 'a list
  val add_bindings : (key * 'a) list -> 'a t -> 'a t
  val fold_map : f:(key -> 'a -> 'b -> 'b * 'c) -> init:'b -> 'a t -> 'b * 'c t
end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  include Caml.Map.Make (Ord)

  let diff equal (a : 'a t) (b : 'a t) =
    fold
      (fun k v m -> if Option.equal equal (Some v) (find_opt k m) then remove k m else m)
      b
      a

  let of_list (lst : (key * 'a) list) : 'a t =
    let aux prev (k, v) = add k v prev in
    List.fold_left ~f:aux ~init:empty lst

  let to_list_rev (t : 'a t) : 'a list =
    let aux _k v prev = v :: prev in
    fold aux t []

  let to_kv_list_rev (t : 'a t) : (key * 'a) list =
    let aux k v prev = (k, v) :: prev in
    fold aux t []

  let to_k_list_rev (t : 'a t) : key list =
    let aux k _v prev = k :: prev in
    fold aux t []

  let to_list l = List.rev @@ to_list_rev l
  let to_kv_list l = List.rev @@ to_kv_list_rev l

  let unzip m =
    let k_lr = to_kv_list_rev m in
    let k, lr = List.unzip k_lr in
    let l, r = List.unzip lr in
    let k_l = List.zip_exn k l in
    let k_r = List.zip_exn k r in
    of_list k_l, of_list k_r

  let keys l = List.rev @@ to_k_list_rev l
  let values l = List.rev @@ to_list_rev l

  let add_bindings (kvl : (key * 'a) list) (m : 'a t) =
    let aux prev (k, v) = add k v prev in
    List.fold_left ~f:aux ~init:m kvl

  let fold_map ~f ~init map =
    let aux k v (init, map) =
      let acc, v = f k v init in
      acc, add k v map
    in
    fold aux map (init, empty)
end

module type SHashable = sig
  include S

  val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state
end

module MakeHashable (Ord : OrderedHashableType) : SHashable with type key = Ord.t = struct
  include Make (Ord)

  let hash_fold_t hash_fold_a hsv arg =
    let sort l = List.sort ~compare:(fun (k, _) (k', _) -> Ord.compare k k') l in
    Hash.Builtin.hash_fold_list
      (fun st (k, v) -> Ord.hash_fold_t (hash_fold_a st v) k)
      hsv
      (sort (to_kv_list arg))
end

module String = Make (String)
