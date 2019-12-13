(** Driver for the LIGO lexer *)

let extension = ".religo"
let options = EvalOpt.read "ReasonLIGO" extension

(** Error printing and exception tracing
*)
let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

(** {1 Preprocessing the input source and opening the input channels} *)

(** Path for CPP inclusions (#include)
*)
let lib_path =
  match options#libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

let prefix =
  match options#input with
    None | Some "-" -> "temp"
  | Some file ->  Filename.(file |> basename |> remove_extension)

let suffix = ".pp" ^ extension

let pp_input =
  if Utils.String.Set.mem "cpp" options#verbose
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match options#input with
    None | Some "-" ->
      Printf.sprintf "cpp -traditional-cpp%s - > %s"
                     lib_path pp_input
  | Some file ->
      Printf.sprintf "cpp -traditional-cpp%s %s > %s"
                     lib_path file pp_input

let () =
  if Utils.String.Set.mem "cpp" options#verbose
  then Printf.eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (Printf.sprintf "the command \"%s\" failed." cpp_cmd)

(** {1 Running the lexer on the input file} *)

module Log = LexerLog.Make (Lexer.Make (LexToken))

let () = Log.trace ~offsets:options#offsets
                   options#mode (Some pp_input) options#cmd
