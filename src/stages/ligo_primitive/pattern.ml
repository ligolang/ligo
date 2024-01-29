module Location = Simple_utils.Location

module type Container = sig
  type 'a t [@@deriving eq, compare, yojson, hash, sexp]

  val iter : 'a t -> f:('a -> unit) -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t
  val to_list : 'a t -> (Label.t * 'a) list
end

module type S = sig
  type 't t [@@deriving eq, compare, yojson, hash]

  val fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val fold_map_pattern : ('a -> 'b t -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t
  val map_pattern : ('a t -> 'b t) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b t -> 'a * 'b t
  val binders : 'a t -> 'a Binder.t list
  val labels : 'a t -> Label.t list
  val var : loc:Location.t -> 'a Binder.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Make (Container : Container) = struct
  type 'ty_exp list_pattern =
    | Cons of 'ty_exp t * 'ty_exp t
    | List of 'ty_exp t list

  and 'ty_exp pattern_repr =
    | P_unit
    | P_var of 'ty_exp Binder.t
    | P_list of 'ty_exp list_pattern
    | P_variant of Label.t * 'ty_exp t
    | P_tuple of 'ty_exp t list
    | P_record of 'ty_exp t Container.t

  and 't t = 't pattern_repr Location.wrap [@@deriving eq, compare, yojson, hash, sexp]

  let var : loc:Location.t -> 'ty Binder.t -> 'ty t =
   fun ~loc b -> Location.wrap ~loc (P_var b)


  let record_pattern : loc:Location.t -> 'ty_exp t Container.t -> 'ty t =
   fun ~loc b -> Location.wrap ~loc (P_record b)


  let variant_pattern : loc:Location.t -> Label.t * 'ty_exp t -> 'ty t =
   fun ~loc (label, pat) -> Location.wrap ~loc (P_variant (label, pat))


  let rec pp_list g ppf pl =
    let mpp = pp g in
    match pl with
    | Cons (pl, pr) -> Format.fprintf ppf "%a::%a" mpp pl mpp pr
    | List pl ->
      Format.fprintf ppf "[%a]" Simple_utils.PP_helpers.(list_sep mpp (tag " ; ")) pl


  and pp type_expression ppf p =
    let open Format in
    match Location.unwrap p with
    | P_unit -> fprintf ppf "()"
    | P_var b -> fprintf ppf "%a" (Binder.pp type_expression) b
    | P_list l -> pp_list type_expression ppf l
    | P_variant (l, p) -> fprintf ppf "%a %a" Label.pp l (pp type_expression) p
    | P_tuple pl ->
      fprintf
        ppf
        "(%a)"
        Simple_utils.PP_helpers.(list_sep (pp type_expression) (tag ","))
        pl
    | P_record lps ->
      let aux ppf (l, p) = fprintf ppf "%a = %a" Label.pp l (pp type_expression) p in
      fprintf
        ppf
        "{ %a }"
        Simple_utils.PP_helpers.(list_sep aux (tag " ; "))
        (Container.to_list lps)


  let rec iter : ('a -> unit) -> 'a t -> unit =
   fun f p ->
    match Location.unwrap p with
    | P_unit -> ()
    | P_var b -> Binder.iter f b
    | P_list (Cons (pa, pb)) ->
      iter f pa;
      iter f pb
    | P_list (List lp) -> List.iter ~f:(iter f) lp
    | P_variant (_, p) -> iter f p
    | P_tuple lp -> List.iter ~f:(iter f) lp
    | P_record lps -> Container.iter ~f:(iter f) lps


  let rec fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a =
   fun f acc p ->
    match Location.unwrap p with
    | P_unit -> f acc p
    | P_var _ -> f acc p
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) -> fold_pattern f (fold_pattern f acc pb) pa
      | List lp -> List.fold_left ~f:(fold_pattern f) ~init:acc lp)
    | P_variant (_, p) -> fold_pattern f acc p
    | P_tuple lp -> List.fold_left ~f:(fold_pattern f) ~init:acc lp
    | P_record lps -> Container.fold ~f:(fold_pattern f) ~init:acc lps


  let rec fold_map_pattern : ('a -> 'b t -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t =
   fun f acc p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_unit -> f acc p
    | P_var _ -> f acc p
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) ->
        let acc, pa = fold_map_pattern f acc pa in
        let acc, pb = fold_map_pattern f acc pb in
        acc, Location.wrap ~loc (P_list (Cons (pa, pb)))
      | List lp ->
        let acc, lp = List.fold_map ~f:(fold_map_pattern f) ~init:acc lp in
        acc, Location.wrap ~loc (P_list (List lp)))
    | P_variant (l, p) ->
      let acc, lp = fold_map_pattern f acc p in
      acc, Location.wrap ~loc (P_variant (l, lp))
    | P_tuple lp ->
      let acc, lp = List.fold_map ~f:(fold_map_pattern f) ~init:acc lp in
      acc, Location.wrap ~loc (P_tuple lp)
    | P_record lps ->
      let acc, lps = Container.fold_map ~f:(fold_map_pattern f) ~init:acc lps in
      acc, Location.wrap ~loc (P_record lps)


  let map_pattern f p = snd @@ fold_map_pattern (fun () x -> (), f x) () p

  let rec fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a =
   fun f acc p ->
    match Location.unwrap p with
    | P_unit -> acc
    | P_var b -> Binder.fold f acc b
    | P_list lp ->
      (match lp with
      | Cons (pa, pb) -> fold f (fold f acc pb) pa
      | List lp -> List.fold_left ~f:(fold f) ~init:acc lp)
    | P_variant (_, p) -> fold f acc p
    | P_tuple lp -> List.fold_left ~f:(fold f) ~init:acc lp
    | P_record lps -> Container.fold ~f:(fold f) ~init:acc lps


  let rec map : ('a -> 'b) -> 'a t -> 'b t =
   fun f p ->
    let self = map f in
    let aux p =
      match p with
      | P_unit -> P_unit
      | P_var b ->
        let b' = Binder.map f b in
        P_var b'
      | P_list lp ->
        let lp =
          match lp with
          | Cons (pa, pb) ->
            let pa = self pa in
            let pb = self pb in
            (Cons (pa, pb) : 'b list_pattern)
          | List lp ->
            let lp = List.map ~f:self lp in
            (List lp : 'b list_pattern)
        in
        P_list lp
      | P_variant (l, p) ->
        let p = self p in
        P_variant (l, p)
      | P_tuple lp ->
        let lp = List.map ~f:self lp in
        P_tuple lp
      | P_record lps ->
        let lps = Container.map ~f:self lps in
        P_record lps
    in
    Location.map aux p


  let rec fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t =
   fun f acc p ->
    let self = fold_map f in
    let ret a wrap_content = a, { p with wrap_content } in
    match p.wrap_content with
    | P_unit -> ret acc P_unit
    | P_var b ->
      let acc, b = Binder.fold_map f acc b in
      ret acc @@ P_var b
    | P_list lp ->
      let acc, lp =
        match lp with
        | Cons (pa, pb) ->
          let acc, pa = self acc pa in
          let acc, pb = self acc pb in
          acc, (Cons (pa, pb) : 'b list_pattern)
        | List lp ->
          let acc, lp = List.fold_map ~f:self ~init:acc lp in
          acc, (List lp : 'b list_pattern)
      in
      ret acc @@ P_list lp
    | P_variant (l, p) ->
      let acc, p = self acc p in
      ret acc @@ P_variant (l, p)
    | P_tuple lp ->
      let acc, lp = List.fold_map ~f:self ~init:acc lp in
      ret acc @@ P_tuple lp
    | P_record lps ->
      let acc, lps = Container.fold_map ~f:self ~init:acc lps in
      ret acc @@ P_record lps


  let binders t =
    fold_pattern
      (fun binders t ->
        match Location.unwrap t with
        | P_var binder -> binder :: binders
        | _ -> binders)
      []
      t


  let labels : 'a t -> Label.t list =
   fun p ->
    let rec aux : 'a t -> Label.t list -> Label.t list =
     fun p acc ->
      match p.wrap_content with
      | P_unit | P_var _ -> acc
      | P_list (Cons (p, ps)) -> aux p @@ aux ps acc
      | P_tuple ps | P_list (List ps) ->
        List.fold_left ~init:acc ~f:(fun acc p -> aux p acc) ps
      | P_variant (label, p) -> label :: aux p acc
      | P_record record ->
        List.fold_left
          ~init:acc
          ~f:(fun acc (label, p) -> label :: aux p acc)
          (Container.to_list record)
    in
    aux p []
end

module Non_linear_pattern = Make (Label.Assoc)
module Linear_pattern = Make (Record)
