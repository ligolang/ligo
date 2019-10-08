(*
type pre_token = {
  name : string ;
  pattern : string ;
}

val make : string -> string -> pre_token

val keyword : string -> pre_token
val symbol : string -> string -> pre_token

module Print_mly : sig
(*
  open Format
  val token : formatter -> pre_token -> unit
  val tokens : formatter -> pre_token list -> unit
*)
end

module Print_mll : sig
(*
  open Format

  val pre : string
  val post : string

  val token : formatter -> pre_token -> unit
  val tokens : formatter -> pre_token list -> unit
*)
end

module Print_ml : sig
(*
  open Format

  val pre : string
  val token : formatter -> pre_token -> unit
  val tokens : formatter -> pre_token list -> unit
*)
end

val tokens : pre_token list
*)
