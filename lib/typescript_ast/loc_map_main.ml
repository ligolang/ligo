open Core
module Region = Simple_utils.Region
module Loc_map = Typescript_ast.Loc_map

let cli_args : string array = Sys.get_argv ()

let new_scan (file : string) : (Loc_map.t, string Region.reg) Result.t =
  try Ok (Loc_map.scan_string (In_channel.read_all file)) with
  | Sys_error msg ->
    let region = Region.min ~file in
    Error Region.{ region; value = msg }

let old_scan (file : string) : (Loc_map.t, string Region.reg) Result.t =
  try
    let in_chan = In_channel.create file
    and init_map : Loc_map.t = Map.set Int.Map.empty ~key:1 ~data:0
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

(* Debug *)

let print map =
  let f ~key ~data = Printf.printf "%i -> %i\n%!" key data in
  Map.iteri ~f map

let () =
  match Array.length cli_args with
  | 2 ->
    let file = cli_args.(1) in
    let () =
      match new_scan file with
      | Ok line_map ->
        Printf.printf "New mapping:\n%!";
        print line_map
      | Error { region = _; value } -> Printf.eprintf "Error: %s\n%!" value
    in
    (match old_scan file with
    | Ok line_map ->
      Printf.printf "Old mapping (reference):\n%!";
      print line_map
    | Error { region = _; value } -> Printf.eprintf "Error: %s\n%!" value)
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
