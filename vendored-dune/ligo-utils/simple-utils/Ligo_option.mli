(* Added functions to Core.Option *)

(* Monadic bind as let syntax *)

val ( let*) : 'a option -> ('a -> 'b option) -> 'b option

(* [map_pair_or (f, g) p] returns the first some-value of [f p, g p],
   or [None] otherwise. *)

val map_pair_or : ('a -> 'b option) * ('a -> 'b option) -> 'a -> 'b option

(* Unzipping an optional pair -- and losing the correlation. *)

val unzip : ('a * 'b) option -> 'a option * 'b option

(* Applying a function to an initial value and an optional value. *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a option -> 'acc * 'b option

(* Equivalent to [Core.Option.all] but on non-empty lists. *)

val nonempty_all : 'a option Nonempty_list.t -> 'a Nonempty_list.t option
