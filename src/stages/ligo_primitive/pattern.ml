module Location = Simple_utils.Location

module Container = struct
  module type S = sig
    type 'a t
    [@@deriving eq,compare,yojson,hash]
    
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val fold_map : ('b -> 'a -> 'b * 'c) -> 'b -> 'a t -> 'b * 'c t
  
    val to_list : 'a t -> (Label.t * 'a) list
    val of_list : (Label.t * 'a) list -> 'a t
  end
  
  module List : S = struct
    type 'a t = (Label.t * 'a) list
    [@@deriving eq,compare,yojson,hash]
  
    let map 
      : ('a -> 'b) -> 'a t -> 'b t 
      = fun f xs -> List.map xs ~f:(fun (l,t) -> (l, f t))
    let fold 
      : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 
      = fun f init xs -> List.fold xs ~f:(fun init (_,t) -> f init t) ~init
    let fold_map 
      : ('b -> 'a -> 'b * 'c) -> 'b -> 'a t -> 'b * 'c t 
      = fun f init xs -> 
          List.fold_map xs 
            ~f:(fun init (l,t) -> 
                  let init, t= f init t in
                  init, (l,t)) ~init
  
    let to_list xs = xs
    let of_list : (Label.t * 'a) list -> 'a t = fun xs -> xs
  end
end

module type S = functor (C : Container.S) ->
  sig 
    type 'ty_exp list_pattern =
      | Cons of 'ty_exp t * 'ty_exp t
      | List of 'ty_exp t list

    and 'ty_exp pattern_repr =
      | P_unit
      | P_var of 'ty_exp Binder.t
      | P_list of 'ty_exp list_pattern
      | P_variant of Label.t * 'ty_exp t
      | P_tuple of 'ty_exp t list
      | P_record of 'ty_exp t C.t

    and 't t = 't pattern_repr Location.wrap
      [@@deriving eq,compare,yojson,hash]

    val fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b t -> 'a * 'b t

    val binders : 'a t -> 'a Binder.t list

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end

module Make(Container : Container.S) = struct
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

and 't t = 't pattern_repr Location.wrap
  [@@deriving eq,compare,yojson,hash]

let rec pp_list g ppf = fun pl ->
  let mpp = pp g in
  match pl with
  | Cons (pl,pr) -> Format.fprintf ppf "%a::%a" mpp pl mpp pr
  | List pl      -> Format.fprintf ppf "[%a]" Simple_utils.PP_helpers.(list_sep mpp (tag " ; ")) pl

and pp type_expression ppf = fun p ->
  let open Format in
  match Location.unwrap p with
  | P_unit   -> fprintf ppf "()"
  | P_var b  -> fprintf ppf "%a" (Binder.pp type_expression) b
  | P_list l -> pp_list type_expression ppf l
  | P_variant (l , p) -> fprintf ppf "%a %a" Label.pp l (pp type_expression) p
  | P_tuple pl ->
    fprintf ppf "(%a)" Simple_utils.PP_helpers.(list_sep (pp type_expression) (tag ",")) pl
  | P_record lps ->
    let aux ppf (l,p) =
      fprintf ppf "%a = %a" Label.pp l (pp type_expression) p
    in
    let kvs = Container.to_list lps in
    fprintf ppf "{ %a }" Simple_utils.PP_helpers.(list_sep aux (tag " ; ")) kvs

let rec fold_pattern : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a =
  fun f acc p ->
    match Location.unwrap p with
    | P_unit -> f acc p
    | P_var _ -> f acc p
    | P_list lp -> (
      match lp with
      | Cons (pa,pb) -> fold_pattern f (fold_pattern f acc pb) pa
      | List lp -> List.fold_left ~f:(fold_pattern f) ~init:acc lp
    )
    | P_variant (_,p) -> fold_pattern f acc p
    | P_tuple lp -> List.fold_left ~f:(fold_pattern f) ~init:acc lp
    | P_record lps -> Container.fold (fold_pattern f) acc lps

let rec fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a =
  fun f acc p ->
    match Location.unwrap p with
    | P_unit -> acc
    | P_var b -> Binder.fold f acc b
    | P_list lp -> (
      match lp with
      | Cons (pa,pb) -> fold f (fold f acc pb) pa
      | List lp -> List.fold_left ~f:(fold f) ~init:acc lp
    )
    | P_variant (_,p) -> fold f acc p
    | P_tuple lp -> List.fold_left ~f:(fold f) ~init:acc lp
    | P_record lps -> Container.fold (fold f) acc lps

let rec map : ('a -> 'b ) -> 'a t -> 'b t =
  fun f p ->
    let self = map f in
    let aux p =
    match p with
    | P_unit -> P_unit
    | P_var b ->
      let b' = Binder.map f b in
      (P_var b')
    | P_list lp -> (
      let lp =
        match lp with
        | Cons (pa,pb) ->
          let pa = self pa in
          let pb = self pb in
          (Cons (pa, pb) : 'b list_pattern)
        | List lp ->
          let lp = List.map ~f:self lp in
          (List lp : 'b list_pattern)
      in
      P_list lp
    )
    | P_variant (l,p) -> (
      let p = self p in
      P_variant (l,p)
    )
    | P_tuple lp ->
      let lp = List.map ~f:self lp in
      P_tuple lp
    | P_record lps ->
      let lps = Container.map self lps in
      P_record lps
    in Location.map aux p

let rec fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t =
  fun f acc p ->
    let self = fold_map f in
    let ret a wrap_content = a,{p with wrap_content} in
    match p.wrap_content with
    | P_unit -> ret acc P_unit
    | P_var b ->
      let acc,b = Binder.fold_map f acc b in
      ret acc @@ P_var b
    | P_list lp -> (
      let acc,lp =
        match lp with
        | Cons (pa,pb) ->
          let acc,pa = self acc pa in
          let acc,pb = self acc pb in
          acc,(Cons (pa, pb) : 'b list_pattern)
        | List lp ->
          let acc,lp = List.fold_map ~f:self ~init:acc lp in
          acc,(List lp : 'b list_pattern)
      in
      ret acc @@ P_list lp
    )
    | P_variant (l,p) -> (
      let acc,p = self acc p in
      ret acc @@ P_variant (l,p)
    )
    | P_tuple lp ->
      let acc,lp = List.fold_map ~f:self ~init:acc lp in
      ret acc @@ P_tuple lp
    | P_record lps ->
      let acc, lps = Container.fold_map self acc lps in
      ret acc @@ P_record lps

let binders t =
  fold_pattern (fun binders t ->
    match Location.unwrap t with
    | P_var binder -> binder :: binders
    | _ -> binders) [] t
end