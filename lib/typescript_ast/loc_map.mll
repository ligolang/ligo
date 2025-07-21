(* Mapping source locations as vertical and horizontal offsets to
   [Pos.t]. See OCaml module [Lexing]. *)

{
(* START HEADER *)

open Core
module Region = Simple_utils.Region

type pos_bol = int
type t = pos_bol Int.Map.t (* From line numbers to beginning-of-line offset *)

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

(* RULES (SCANNERS) *)

rule scan map lnum bol = parse
  '\n' { let key, data = lnum + 1, bol + 1 in
         let map = Map.set ~key ~data map in
         scan map key data lexbuf }
| eof  { map }
| _    { scan map lnum (bol + 1) lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let scan_string (string : string) : t =
  let lexbuf = Lexing.from_string string in
  let init_map : t = Map.set Int.Map.empty ~key:1 ~data:0 in
  scan init_map 1 0 lexbuf

let scan_buffer buffer : t = scan_string (Buffer.contents buffer)

let convert file map v_offset h_offset : Lexing.position option =
  let line = v_offset + 1 in
  match Map.find map line with
  | None -> None
  | Some pos_bol ->
    Some { pos_fname = file; pos_lnum = line; pos_bol; pos_cnum = pos_bol + h_offset }

(* END TRAILER *)
}
