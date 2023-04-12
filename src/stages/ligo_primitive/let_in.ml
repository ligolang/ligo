module type Attr = sig
  type t [@@deriving eq, compare, yojson, hash]

  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type 'a t [@@deriving eq, compare, yojson, hash, fold, map]

  val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b t -> 'a * 'b t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Make (Pattern : S) (Attr : Attr) = struct
  type ('e, 't) t =
    { let_binder : 't Pattern.t
    ; rhs : 'e
    ; let_result : 'e
    ; attributes : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp f g ppf { let_binder; rhs; let_result; attributes = attr } =
    Format.fprintf
      ppf
      "@[<v>let %a = %a%a in@,%a@]"
      (Pattern.pp g)
      let_binder
      f
      rhs
      Attr.pp
      attr
      f
      let_result


  let pp_mut f g ppf { let_binder; rhs; let_result; attributes = attr } =
    Format.fprintf
      ppf
      "@[<v>let mut %a = %a%a in@,%a@]"
      (Pattern.pp g)
      let_binder
      f
      rhs
      Attr.pp
      attr
      f
      let_result
end
