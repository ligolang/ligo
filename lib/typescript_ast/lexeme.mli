(* Reading in the source the text at a given region (expecting a
   lexeme) *)

module Region = Simple_utils.Region

val open_input : file:string -> unit
val close_input : unit -> unit
val read : Region.t -> string
