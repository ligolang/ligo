(* Patching .ligo extension to be .jsligo in #include and #import *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std

(* Utilities *)

type tokens = Token.t list

(* Filter *)

let filter (tokens : tokens) =
  let open! Token
  in
  let rec aux acc = function
    (Directive (PP_Include d) as dir) :: tokens -> (
      let name = d#file_path.value in
      let prefix = Caml.Filename.chop_suffix_opt ~suffix:".ligo" name
      in match prefix with
           None -> aux (dir :: acc) tokens
         | Some prefix ->
             let path = {d#file_path with value = prefix ^ ".jsligo"} in
             let d'   = Directive.PP_Include (d#set_file_path path)
             in aux (Directive d' :: acc) tokens
    )
  | (Directive (PP_Import d) as dir) :: tokens -> (
      let name = d#file_path.value in
      let prefix = Caml.Filename.chop_suffix_opt ~suffix:".ligo" name
      in match prefix with
           None -> aux (dir :: acc) tokens
         | Some prefix ->
             let path = {d#file_path with value = prefix ^ ".jsligo"} in
             let d'   = Directive.PP_Import (d#set_file_path path)
             in aux (Directive d' :: acc) tokens
    )
  | token :: tokens -> aux (token :: acc) tokens
  | [] -> List.rev acc (* We restore the original order. *)
  in
  aux [] tokens

(* Exported *)

type message = string Region.reg

let filter :
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      tokens ->
      (tokens, tokens * message) result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running PascaLIGO token self-pass: \
              Patching #include and #import.")
    | None -> ()
  in Ok (filter tokens)
