(*
Ligo_string represent string as they are writen in a ligo program,
delimited either with double quotes (standard) or with `{|...|}` (verbatim)
*)

type t =
  Standard of string
| Verbatim of string [@@deriving hash, sexp]

val standard : string -> t
val verbatim : string -> t

val compare : ?compare:(string->string->int) -> t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val extract : t -> string

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t,string) result

val get_type : t -> [`Verbatim | `Standard]
