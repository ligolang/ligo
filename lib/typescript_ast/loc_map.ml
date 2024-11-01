(* Mapping source locations as vertical and horizontal offsets to
   [Pos.t]. See OCaml module [Lexing]. *)

open Core
module Region = Simple_utils.Region

type pos_bol = int
type t = pos_bol Int.Map.t (* From line numbers to beginning-of-line offset *)

let scan (file : string) : (t, string Region.reg) Result.t =
  try
    let in_chan = In_channel.create file
    and init_map : t = Map.set Int.Map.empty ~key:1 ~data:0
    and f (lnum, bol, map) line =
      let bol = bol + String.length line + 1 in
      lnum + 1, bol, Map.set map ~key:lnum ~data:bol
    in
    let _, _, map = In_channel.fold_lines in_chan ~init:(2, 0, init_map) ~f in
    let () = In_channel.close in_chan in
    Ok map
  with
  | Sys_error msg ->
    let region = Region.min ~file in
    Error Region.{ region; value = msg }

let convert file map v_offset h_offset : Lexing.position option =
  let line = v_offset + 1 in
  match Map.find map line with
  | None -> None
  | Some pos_bol ->
    Some { pos_fname = file; pos_lnum = line; pos_bol; pos_cnum = pos_bol + h_offset }
