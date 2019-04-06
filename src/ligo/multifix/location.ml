type file_location = {
  filename : string ;
  start_line : int ;
  start_column : int ;
  end_line : int ;
  end_column : int ;
}

type virtual_location = string

type t =
  | File of file_location
  | Virtual of virtual_location

let make (start_pos:Lexing.position) (end_pos:Lexing.position) : t =
  let filename = start_pos.pos_fname in
  let start_line = start_pos.pos_lnum in
  let end_line = end_pos.pos_lnum in
  let start_column = start_pos.pos_cnum - start_pos.pos_bol in
  let end_column = end_pos.pos_cnum - end_pos.pos_bol in
  File { filename ; start_line ; start_column ; end_line ; end_column }

let virtual_location s = Virtual s
let dummy = virtual_location "dummy"

