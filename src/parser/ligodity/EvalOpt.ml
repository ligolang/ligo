(* Parsing the command-line option for CameLIGO *)

type options = {
  input   : string option;
  libs    : string list;
  verbose : Utils.String.Set.t
}

let abort msg =
  Utils.highlight (Printf.sprintf "Command-line error: %s" msg); exit 1

let printf  = Printf.printf
let sprintf = Printf.sprintf
let print   = print_endline

(* Help *)

let help () =
  let file = Filename.basename Sys.argv.(0) in
  printf "Usage: %s [<option> ...] [<input>.mligo | \"-\"]\n" file;
  print "where <input>.mligo is the CameLIGO source file (default: stdin),";
  print "and each <option> (if any) is one of the following:";
  print "  -I <paths>             Library paths (colon-separated)";
  print "      --verbose=<phases> Colon-separated phases: cmdline, lexer, parser";
  print "      --version          Send short commit hash to stdout";
  print "  -h, --help             This help";
  exit 0

(* Version *)

let version () = printf "%s\n" Version.version; exit 0

(* Specifying the command-line options a la GNU *)

let input     = ref None
and verbose   = ref Utils.String.Set.empty
and libs      = ref []
and verb_str  = ref ""

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let add_verbose d =
  verbose := List.fold_left (Utils.swap Utils.String.Set.add)
                            !verbose
                            (split_at_colon d)

let specs =
  let open! Getopt in [
    'I',     nolong,      None, Some add_path;
    noshort, "verbose",   None, Some add_verbose;
    'h',     "help",      Some help, None;
    noshort, "version",   Some version, None
  ]
;;

(* Handler of anonymous arguments *)

let anonymous arg =
  match !input with
      None -> input := Some arg
  | Some _ -> abort (sprintf "Multiple inputs")

(* Checking options *)

let string_of convert = function
    None -> "None"
| Some s -> sprintf "Some %s" (convert s)

let string_of_path p =
  let apply s a = if a = "" then s else s ^ ":" ^ a
  in List.fold_right apply p ""

let quote s = Printf.sprintf "\"%s\"" s

let print_opt () =
  printf "COMMAND LINE\n";
  printf "input     = %s\n" (string_of quote !input);
  printf "verbose   = %s\n" !verb_str;
  printf "libs      = %s\n" (string_of_path !libs)

let check () =
  let () =
    if Utils.String.Set.mem "cmdline" !verbose then print_opt () in

  let input =
    match !input with
      None | Some "-" -> !input
    | Some file_path ->
        if   Filename.check_suffix file_path ".mligo"
        then if   Sys.file_exists file_path
             then Some file_path
             else abort "Source file not found."
        else abort "Source file lacks the extension .mligo." in

  (* Exporting remaining options as non-mutable values *)

  let verbose = !verbose
  and libs    = !libs in

  let () =
    if Utils.String.Set.mem "cmdline" verbose then
      begin
        printf "\nEXPORTED COMMAND LINE\n";
        printf "input     = %s\n" (string_of quote input);
        printf "verbose   = %s\n" !verb_str;
        printf "libs      = %s\n" (string_of_path libs)
    end

  in {input; libs; verbose}

(* Parsing the command-line options *)

let read () =
  try
    Getopt.parse_cmdline specs anonymous;
    (verb_str :=
       let apply e a =
         if a <> "" then Printf.sprintf "%s, %s" e a else e
       in Utils.String.Set.fold apply !verbose "");
    check ()
  with Getopt.Error msg -> abort msg
