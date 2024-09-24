type t =
  { path : Filename.t
  ; impl : Ast_typed.module_
  }
[@@deriving bin_io]

module Serialized : sig
  val input : Filename.t -> t option
  val output : t -> unit
  (** Makes cmo path from original file path *)
  val make_path : Filename.t -> Filename.t
end
