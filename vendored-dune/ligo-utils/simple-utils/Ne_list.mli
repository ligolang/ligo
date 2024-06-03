open Core

type 'a t = 'a Nonempty_list.t = (::) of 'a * 'a list [@@deriving eq, compare, yojson, hash, sexp, fold, map, iter]

val make : 'a -> 'a list -> 'a t
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

val of_list_opt : 'a list -> 'a t option
val append : 'a t -> 'a t -> 'a t
val collect : 'a option t -> 'a t option

val fold_right1 : f:('a -> 'a -> 'a) -> 'a t -> 'a

type json = Yojson.Safe.t

val yojson_of_t : ('a -> json) -> 'a t -> json
