(* Reading in the source the text at a given region (expecting a
   lexeme) *)

open Core
module Region = Simple_utils.Region

let read (buffer : Buffer.t) (region : Region.t) : string =
  let start_pos, stop_pos = region#byte_pos in
  let start_cnum = start_pos.Lexing.pos_cnum
  and stop_cnum = stop_pos.Lexing.pos_cnum in
  let len = stop_cnum - start_cnum in
  let bytes = Buffer.sub buffer ~pos:start_cnum ~len in
  Bytes.to_string bytes
