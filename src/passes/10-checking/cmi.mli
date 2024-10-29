module Location = Simple_utils.Location

(** Used to verify cmi consistency *)
type crc = Md5.t [@@deriving bin_io]

(** Record representing cmi file contents *)
type t =
  { imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }
[@@deriving bin_io]

(** Returns cmi's deps list without crcs *)
val get_deps : t -> Filename.t list

module Serialized : sig
  (** Reads file in provided path and tries to parse it as cmi *)
  val input : Filename.t -> (t * crc) option
  (** Outputs cmi object in provided path *)
  val output : t -> Filename.t -> unit
  (** Checks if file on provided path contains serialized cmi *)
  val is_cmi : Filename.t -> bool
  (** Makes cmi path from original file path
      "src/test/contracts/id.mligo" -> "src/test/contracts/id.cmi" *)
  val of_file_name : Filename.t -> Filename.t
  (** Calculates crc of cmi record *)
  val compute_crc : t -> crc
end
