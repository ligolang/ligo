(*
Ligo_string represent string as they are writen in a ligo program,
delimited either with double quotes (standard) or with `{|...|}` (Varbatim)
*)

type t =
  Standard of string
| Verbatim of string

val pp : Format.formatter -> t -> unit
val extract : t -> string
