module Make(Ord : Map.OrderedType) = struct
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
