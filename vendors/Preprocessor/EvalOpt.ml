(* Parsing command-line options *)

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
  ext     : string
>

let make ~input ~libs ?block ?line ~offsets ~verbose ~ext : options =
  object
    method input   = input
    method libs    = libs
    method block   = block
    method line    = line
    method offsets = offsets
    method verbose = verbose
    method ext     = ext
  end

(* Auxiliary functions and modules *)

let  printf = Printf.printf
let sprintf = Printf.sprintf
let   print = print_endline

(* Printing a string in red to standard error *)

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

(* Failure *)

let abort msg =
  highlight (sprintf "Command-line error: %s\n" msg); exit 1

(* Help *)

let help ext () =
  let file = Filename.basename Sys.argv.(0) in
  printf "Usage: %s [<option> ...] [<input>%s | \"-\"]\n" file ext;
  printf "where <input>%s is the LIGO source file (default: stdin),\n" ext;
  print "and each <option> (if any) is one of the following:";
  print "  -I <paths>             Inclusion paths (colon-separated)";
  print "      --columns          Columns for source locations";
  print "      --verbose=<stages> preproc";
  print "  -h, --help             This help";
  exit 0

(* Specifying the command-line options a la GNU *)

let input    = ref None
and libs     = ref []
and columns  = ref false
and verbose  = ref SSet.empty
and verb_str = ref ""

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let add_verbose d =
  verbose := List.fold_left (fun x y -> SSet.add y x)
                            !verbose
                            (split_at_colon d)
let specs ext =
  let open! Getopt in [
    'I',     nolong,    None, Some add_path;
    'h',     "help",    Some (help ext), None;
    noshort, "columns", set columns true, None;
    noshort, "verbose", None, Some add_verbose
  ]

(* Handler of anonymous arguments *)

let anonymous arg =
  match !input with
      None -> input := Some arg
  | Some _ -> abort (sprintf "Multiple inputs")

(* Checking options and exporting them as non-mutable values *)

let check ?block ?line ~ext =
  let libs = !libs

  and offsets = not !columns

  and verbose = !verbose

  and input =
    match !input with
      None | Some "-" -> None
    | Some file_path ->
        if   Filename.check_suffix file_path ext
        then if   Sys.file_exists file_path
             then Some file_path
             else abort "Source file not found."
        else abort ("Source file lacks the extension " ^ ext ^ ".")

 in make ~input ~libs ?block ?line ~offsets ~verbose ~ext

(* Parsing the command-line options *)

type extension = string

let read ?block ?line (ext: extension) =
  try
    Getopt.parse_cmdline (specs ext) anonymous;
    (verb_str :=
       let apply e a =
         if a = "" then e else sprintf "%s, %s" e a
       in SSet.fold apply !verbose "");
    check ?block ?line ~ext
  with Getopt.Error msg -> abort msg
