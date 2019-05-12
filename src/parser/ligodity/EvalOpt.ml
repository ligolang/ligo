(* Parsing the command-line option for the Mini-ML compiler/interpreter *)

let abort msg =
  Utils.highlight (Printf.sprintf "Command-line error: %s" msg); exit 1

let printf  = Printf.printf
let sprintf = Printf.sprintf

(* Help *)

let help () =
  let file = Filename.basename Sys.argv.(0) in
  printf "Usage: %s [<option> ...] [<input>.ml | \"-\"]\n" file;
  print_endline "where <input>.mml is the Mini-ML source file (default: stdin),";
  print_endline "and each <option> (if any) is one of the following:";
  print_endline "  -I <paths>             Library paths (colon-separated)";
  print_endline "  -c [<file>.ml]         Translate to OCaml in <file>.ml";
  print_endline "                         (default: <input>.ml)";
  print_endline "  -e, --eval             Interpret <input>.mml or stdin";
  print_endline "      --raw-edits        Do not optimise translation edits";
  print_endline "      --verbose=<phases> Colon-separated phases: cmdline, lexer,";
  print_endline "                         parser, unparsing, norm, eval";
  print_endline "      --version          Short commit hash on stdout";
  print_endline "  -h, --help             This help";
  exit 0

(* Version *)

let version () = printf "%s\n" Version.version; exit 0

(* Specifying the command-line options a la GNU *)

let input     = ref None
and eval      = ref false
and compile   = ref None
and verbose   = ref Utils.String.Set.empty
and libs      = ref []
and raw_edits = ref false

let set_opt var err =
  Some (fun x -> if !var = None then var := Some x else raise (Getopt.Error err))

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let add_verbose d =
  verbose := List.fold_left (Utils.swap Utils.String.Set.add)
                            !verbose
                            (split_at_colon d)

let specs =
  let open! Getopt in [
    'I',     nolong,      None, Some add_path;
    'c',     nolong,      set compile (Some ""),
                          set_opt compile "Multiple OCaml outputs";
    'e',     "eval",      set eval true, None;
    noshort, "raw-edits", set raw_edits true, None;
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

(* Parsing the command-line options *)

let () = try Getopt.parse_cmdline specs anonymous with
           Getopt.Error msg -> abort msg

(* Checking options *)

let string_of convert = function
    None -> "None"
| Some s -> sprintf "Some %s" (convert s)

let string_of_path p =
  let apply s a = if a = "" then s else s ^ ":" ^ a
  in List.fold_right apply p ""

let quote s = Printf.sprintf "\"%s\"" s

let verb_str =
  let apply e a =
    if a <> "" then Printf.sprintf "%s, %s" e a else e
  in Utils.String.Set.fold apply !verbose ""

let print_opt () =
  printf "COMMAND LINE\n";
  printf "input     = %s\n" (string_of quote !input);
  printf "compile   = %s\n" (string_of quote !compile);
  printf "eval      = %B\n" !eval;
  printf "raw_edits = %b\n" !raw_edits;
  printf "verbose   = %s\n" verb_str;
  printf "libs      = %s\n" (string_of_path !libs)

let () = if Utils.String.Set.mem "cmdline" !verbose then print_opt ()

let input =
  match !input with
    None | Some "-" ->
     if !compile <> None then
       abort "An input file is missing (for compilation)."
     else !input
  | Some file_path ->
      if   Filename.check_suffix file_path ".mml"
      then if   Sys.file_exists file_path
           then Some file_path
           else abort "Source file not found."
      else abort "Source file lacks the extension .mml."

let compile =
  match !compile with
    Some _ when !eval -> abort "Options -e and -c are mutually exclusive."
  | None | Some "-" -> !compile
  | Some "" ->
     (match input with
        None | Some "-" -> abort "The target OCaml filename is missing."
      | Some file -> Some (Filename.remove_extension file ^ ".ml"))
  | Some compile' ->
      if   Filename.check_suffix compile' ".ml"
      then !compile
      else abort "The extension of the target OCaml file is not .ml"

(* Exporting remaining options as non-mutable values *)

let eval      = !eval
and verbose   = !verbose
and libs      = !libs
and raw_edits = !raw_edits

let () =
  if Utils.String.Set.mem "cmdline" verbose then
    begin
      printf "\nEXPORTED COMMAND LINE\n";
      printf "input     = %s\n" (string_of quote input);
      printf "compile   = %s\n" (string_of quote compile);
      printf "eval      = %B\n" eval;
      printf "raw_edits = %B\n" raw_edits;
      printf "verbose   = %s\n" verb_str;
      printf "I         = %s\n" (string_of_path libs)
    end
