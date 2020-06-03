(* Parsing command-line options *)

(* The type [command] denotes some possible behaviours of the
    compiler. *)

type command = Quiet | Copy | Units | Tokens

(* The type [options] gathers the command-line options. *)

module SSet = Set.Make (String)

type line_comment = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

let mk_block ~opening ~closing : block_comment =
  object
    method opening = opening
    method closing = closing
  end

type options = <
  input   : string option;
  libs    : string list;
  verbose : SSet.t;
  offsets : bool;
  block   : block_comment option;
  line    : line_comment option;
  ext     : string;
  mode    : [`Byte | `Point];
  cmd     : command;
  mono    : bool;
  expr    : bool
>

let make ~input ~libs ~verbose ~offsets ?block
         ?line ~ext ~mode ~cmd ~mono ~expr : options =
  object
    method input   = input
    method libs    = libs
    method verbose = verbose
    method offsets = offsets
    method block   = block
    method line    = line
    method ext     = ext
    method mode    = mode
    method cmd     = cmd
    method mono    = mono
    method expr    = expr
  end

(* Auxiliary functions *)

let  printf = Printf.printf
let sprintf = Printf.sprintf
let   print = print_endline

(* Printing a string in red to standard error *)

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let abort msg =
  highlight (sprintf "Command-line error: %s\n" msg); exit 1

(* Help *)

let help extension () =
  let file = Filename.basename Sys.argv.(0) in
  printf "Usage: %s [<option> ...] [<input>%s | \"-\"]\n" file extension;
  printf "where <input>%s is the LIGO source file (default: stdin),\n" extension;
  print "and each <option> (if any) is one of the following:";
  print "  -I <paths>             Library paths (colon-separated)";
  print "  -t, --tokens           Print tokens";
  print "  -u, --units            Print lexical units";
  print "  -c, --copy             Print lexemes and markup";
  print "  -q, --quiet            No output, except errors (default)";
  print "      --columns          Columns for source locations";
  print "      --bytes            Bytes for source locations";
  print "      --mono             Use Menhir monolithic API";
  print "      --expr             Parse an expression";
  print "      --verbose=<stages> cli, preproc, ast-tokens, ast (colon-separated)";
  print "      --version          Commit hash on stdout";
  print "  -h, --help             This help";
  exit 0

(* Version *)

let version () = printf "%s\n" Version.version; exit 0

(* Specifying the command-line options a la GNU *)

let copy     = ref false
and tokens   = ref false
and units    = ref false
and quiet    = ref false
and columns  = ref false
and bytes    = ref false
and verbose  = ref SSet.empty
and input    = ref None
and libs     = ref []
and verb_str = ref ""
and mono     = ref false
and expr     = ref false

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let add_verbose d =
  verbose := List.fold_left (fun x y -> SSet.add y x)
                            !verbose
                            (split_at_colon d)

let specs extension =
  let open! Getopt in [
    'I',     nolong,    None, Some add_path;
    'c',     "copy",    set copy true, None;
    't',     "tokens",  set tokens true, None;
    'u',     "units",   set units true, None;
    'q',     "quiet",   set quiet true, None;
    noshort, "columns", set columns true, None;
    noshort, "bytes",   set bytes true, None;
    noshort, "mono",    set mono true, None;
    noshort, "expr",    set expr true, None;
    noshort, "verbose", None, Some add_verbose;
    'h',     "help",    Some (help extension), None;
    noshort, "version", Some version, None
  ]
;;

(* Handler of anonymous arguments *)

let anonymous arg =
  match !input with
      None -> input := Some arg
  | Some _ -> abort (sprintf "Multiple inputs")

(* Checking options and exporting them as non-mutable values *)

let string_of convert = function
    None -> "None"
| Some s -> sprintf "Some %s" (convert s)

let string_of_path p =
  let apply s a = if a = "" then s else s ^ ":" ^ a
  in List.fold_right apply p ""

let quote s = sprintf "\"%s\"" s

let print_opt () =
  printf "COMMAND LINE\n";
  printf "copy     = %b\n" !copy;
  printf "tokens   = %b\n" !tokens;
  printf "units    = %b\n" !units;
  printf "quiet    = %b\n" !quiet;
  printf "columns  = %b\n" !columns;
  printf "bytes    = %b\n" !bytes;
  printf "mono     = %b\n" !mono;
  printf "expr     = %b\n" !expr;
  printf "verbose  = %s\n" !verb_str;
  printf "input    = %s\n" (string_of quote !input);
  printf "libs     = %s\n" (string_of_path !libs)

let check ?block ?line ~ext =
  let () =
    if SSet.mem "cli" !verbose then print_opt () in

  let input =
    match !input with
      None | Some "-" -> None
    | Some file_path ->
        if   Filename.check_suffix file_path ext
        then if   Sys.file_exists file_path
             then Some file_path
             else abort "Source file not found."
        else abort ("Source file lacks the extension " ^ ext ^ ".") in

  (* Exporting remaining options as non-mutable values *)

  let copy    = !copy
  and tokens  = !tokens
  and units   = !units
  and quiet   = !quiet
  and offsets = not !columns
  and mode    = if !bytes then `Byte else `Point
  and mono    = !mono
  and expr    = !expr
  and verbose = !verbose
  and libs    = !libs in

  let () =
    if SSet.mem "cli" verbose then
      begin
        printf "\nEXPORTED COMMAND LINE\n";
        printf "copy     = %b\n" copy;
        printf "tokens   = %b\n" tokens;
        printf "units    = %b\n" units;
        printf "quiet    = %b\n" quiet;
        printf "offsets  = %b\n" offsets;
        printf "mode     = %s\n" (if mode = `Byte then "`Byte" else "`Point");
        printf "mono     = %b\n" mono;
        printf "expr     = %b\n" expr;
        printf "verbose  = %s\n" !verb_str;
        printf "input    = %s\n" (string_of quote input);
        printf "libs     = %s\n" (string_of_path libs)
      end in

  let cmd =
    match quiet, copy, units, tokens with
      false, false, false, false
    |  true, false, false, false -> Quiet
    | false,  true, false, false -> Copy
    | false, false,  true, false -> Units
    | false, false, false,  true -> Tokens
    | _ -> abort "Choose one of -q, -c, -u, -t."

  in make ~input ~libs ~verbose ~offsets ~mode
          ~cmd ~mono ~expr ?block ?line ~ext

(* Parsing the command-line options *)

type extension = string

let read ?block ?line (ext: extension) =
  try
    Getopt.parse_cmdline (specs ext) anonymous;
    (verb_str :=
       let apply e a =
         if a = "" then e else Printf.sprintf "%s, %s" e a
       in SSet.fold apply !verbose "");
    check ?block ?line ~ext
  with Getopt.Error msg -> abort msg
