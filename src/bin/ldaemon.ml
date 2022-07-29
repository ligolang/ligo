open Cli_helpers
(* 
let unpack0 f = function [] -> f () | lst -> prerr_endline (Format.asprintf "expected 0 arg got %d" (List.length lst))
let unpack1 f = function [a] -> f a | lst -> prerr_endline (Format.asprintf "expected 1 arg got %d" (List.length lst))
let unpack2 f = function [a;b] -> f a b | lst -> prerr_endline (Format.asprintf "expected 2 arg got %d" (List.length lst))
let unpack3 f = function [a;b;c] -> f a b c | lst -> prerr_endline (Format.asprintf "expected 3 arg got %d" (List.length lst))

(* let unpack i =
  match i with
  | 0 -> unpack0 | 1 -> unpack1 | 2 -> unpack2 | 3 -> unpack3 | _ -> failwith "lol" *)

module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options
let return = ref Done

let command_regexp = Str.regexp "\\([a-z\\-]+\\)\\([a-zA-Z0-9\\.]*[ \x0c\t]*\\)*"


let get_scope = unpack1 @@ fun fname ->
  let raw_options = Raw_options.make ~libraries:[] ~with_types:true () in
  return_result ~return @@
    Ligo_api.Info.get_scope raw_options fname  (Ex_display_format Json :Simple_utils.Display.ex_display_format)

type callbacks = string list -> unit
let command_map = [
  ("get-scope", (1, get_scope) );
  ("dummy"     , (0, (fun _ -> print_endline "dummy output")));
]


let get_all_groups s =
  let rec aux i =
    try
      print_endline (string_of_int i);
      let g = Str.matched_group i s in
      print_endline g;
      g::(aux (i+1))
    with Caml.Not_found -> []
  in
  let grps = List.rev (aux 1) in
  print_endline "-----";
  List.iter grps ~f:(print_endline);
  print_endline "-----";
  grps

let match_command_name str =
  if Str.string_match command_regexp str 0 then
    let command_name = Str.matched_group 1 str in
    match List.Assoc.find ~equal:String.equal command_map command_name with
    | None -> print_endline "unknown command"
    | Some (i,cb) ->
      let params = get_all_groups str in
      let () = if not(List.length params = i) then print_endline "wrong number of argument" in
      cb params
  else print_endline "invalid command syntax"
 *)

let run_ligo ~reset_return ~(run : ?argv:string list -> unit -> int) args =
  Ast_core.ValueVar.reset_counter ();
  Ast_core.TypeVar.reset_counter ();
  Ast_core.ModuleVar.reset_counter ();
  Self_ast_aggregated.reset_counter ();
  reset_return ();
  let argv = ("ligo" :: args) in
  let result = run ~argv () in
  result


(* let _maybe_useful = "\x0" *)
let main ~reset_return ~(run : ?argv:string list -> unit -> int) () =
  let stdin = In_channel.stdin in
  while true do    
    let x = Stdio.In_channel.input_line stdin in
    (* get_char '\0' *)
    (* \0 *)
    (* In_channel.really_input_exn *)
    let x = match x with 
      Some s ->
        (* let () = match_command_name s in *)
        let args = String.split_on_chars s ~on:['\n';'\t';' '] in
        (* let args = List.map ~f:(fun s -> 
          if String.get s 0 = '"' || s.[0] = '\'' then
            ""
          else "" 
          ) args in *)
          let args =
            (* Handle single & double quotes *)
            (* let f1 = Staged.unstage (String.Escaping.unescape ~escape_char:'\'') in
            let f2 = Staged.unstage (String.Escaping.unescape ~escape_char:'"') in *)
            List.map args ~f:(fun x -> 
              let f s = if Char.equal (String.get s 0) '\"' || Char.equal (String.get s 0) '\'' 
                then Caml.String.sub s 1 (String.length s - 2) else s in
              (* f1 x |> f2 *)
              (* failwith "l" *)
              f x
              (* 
              
val is_char_literal : string -> escape_char:char -> int -> bool
is_literal s ~escape_char pos return true if the char at pos is not escaped or escaping.
                    
                    *)
              )
          in
          (* val unescape : escape_char:char -> (string -> string) Staged.t *)
          (* let () = List.iter args ~f:print_endline in *)
          let _ = run_ligo args ~reset_return ~run in
        ""
    | None -> "could not read" in
    print_endline x;
  done;
  Ok ("","")


