module Make (Path : Access_path.S) = struct
  type 'e t =
    { struct_ : 'e
    ; path : 'e Path.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map, iter, sexp]

  let pp f ppf ({ struct_; path } : _ t) =
    Format.fprintf ppf "%a.%a" f struct_ (Path.pp f) path


  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
   fun f acc { struct_; path = p } ->
    let acc, struct_ = f acc struct_ in
    let acc, path = Path.fold_map f acc p in
    acc, { struct_; path }
end
