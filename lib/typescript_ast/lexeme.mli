(* Reading in the source the text at a given region (expecting a
   lexeme) *)

module Region = Simple_utils.Region

val read : Buffer.t -> Region.t -> string
