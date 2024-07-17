module type S1 = sig
  type 'a t [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val fold_map : ('acc -> 'a1 -> 'acc * 'a2) -> 'acc -> 'a1 t -> 'acc * 'a2 t
end

module type S2 = sig
  type ('a, 'b) t [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]

  val pp
    :  (Format.formatter -> 'a -> unit)
    -> (Format.formatter -> 'b -> unit)
    -> Format.formatter
    -> ('a, 'b) t
    -> unit

  val fold_map
    :  ('acc -> 'a1 -> 'acc * 'a2)
    -> ('acc -> 'b1 -> 'acc * 'b2)
    -> 'acc
    -> ('a1, 'b1) t
    -> 'acc * ('a2, 'b2) t
end
