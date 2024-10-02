type crc = Md5.t [@@deriving bin_io]

type t =
  { path : Filename.t
  ; imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }
[@@deriving bin_io]

val get_deps : t -> Filename.t list

module Serialized : sig
  val input : Filename.t -> (t * crc) option
  val output : t -> unit
  (** Makes cmi path from original file path *)
  val make_path : Filename.t -> Filename.t
  val compute_crc : t -> crc
end
