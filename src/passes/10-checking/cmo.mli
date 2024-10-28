type t =
  { impl : Ast_typed.module_
  }
[@@deriving bin_io]

module Serialized : sig
  val input : Filename.t -> t option
  val output : t -> Filename.t -> unit
  val is_cmo : Filename.t -> bool
  (** Makes cmo path from original file path *)
  val of_file_name : Filename.t -> Filename.t
end
