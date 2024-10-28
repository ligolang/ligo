module Location = Simple_utils.Location
type crc = Md5.t [@@deriving bin_io]

type t =
  { imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }
[@@deriving bin_io]

val get_deps : t -> Filename.t list

module Serialized : sig
  val input : Filename.t -> (t * crc) option
  val output : t -> Filename.t -> unit
  val is_cmi : Filename.t -> bool
  (** Makes cmi path from original file path *)
  val of_file_name : Filename.t -> Filename.t
  val compute_crc : t -> crc
end
