include module type of Semver

type t = int * int * int [@@deriving yojson]

val to_string : t -> string
val of_string : string -> t option
val compare : t -> t -> int
val equal : t -> t -> bool
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
