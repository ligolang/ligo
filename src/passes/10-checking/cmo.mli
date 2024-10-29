(** Record representing cmo file contents *)
type t =
  { impl : Ast_typed.module_
  }
[@@deriving bin_io]

module Serialized : sig
  (** Reads file in provided path and tries to parse it as cmo *)
  val input : Filename.t -> t option
  (** Outputs cmo object in provided path *)
  val output : t -> Filename.t -> unit
  (** Checks if file on provided path contains serialized cmo *)
  val is_cmo : Filename.t -> bool
  (** Makes cmi path from original file path
      "src/test/contracts/id.mligo" -> "src/test/contracts/id.cmo" *)
  val of_file_name : Filename.t -> Filename.t
end
