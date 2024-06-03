open Core

type t = Z.t [@@deriving equal, compare]

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) Result.t

val error_yojson_format : string -> ('a, string) Result.t

val sexp_of_t : t -> Sexp.t

(* Can raise ["[Ligo_z.t_of_sexp] Invalid sexp encoding"] *)
val t_of_sexp : Sexp.t -> t

val hash_fold_t :
  Base_internalhash_types.state -> t -> Base_internalhash_types.state
