module Make (Path : Access_path.S) = struct
  type 'e t =
    { struct_ : 'e
    ; path : 'e Path.t
    ; update : 'e
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp f ppf ({ struct_; path; update } : _ t) =
    Format.fprintf ppf "{ %a with %a = %a }" f struct_ (Path.pp f) path f update


  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
   fun f acc { struct_; path = p; update } ->
    let acc, struct_ = f acc struct_ in
    let acc, path = Path.fold_map f acc p in
    let acc, update = f acc update in
    acc, { struct_; path; update }
end
