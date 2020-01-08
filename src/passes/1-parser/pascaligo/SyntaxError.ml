type t =
  Reserved_name of string Region.reg
| Duplicate_parameter of string Region.reg
| Duplicate_variant of string Region.reg

type error = t

exception Error of t
