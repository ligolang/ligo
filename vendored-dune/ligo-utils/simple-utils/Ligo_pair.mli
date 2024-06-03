(* Functions operating on pairs of values *)

val compare : ('a -> 'b -> int) -> ('c -> 'd -> int) -> 'a * 'c -> 'b * 'd -> int

val map : f:('a -> 'b) -> 'a * 'a -> 'b * 'b

val map_fst : f:('a -> 'b) -> 'a * 'c -> 'b * 'c

val map_snd : f:('a -> 'b) -> 'c * 'a -> 'c * 'b

val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b * 'b -> 'a

val fold_map : f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b * 'b -> 'a * ('c * 'c)
