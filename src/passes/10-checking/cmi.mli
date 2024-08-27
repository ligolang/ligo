type crc = Md5.t

type t =
  { path : Filename.t
  ; imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }

module Serialized : sig
  val input : Filename.t -> (t * crc) option
  val output : t -> crc
end
