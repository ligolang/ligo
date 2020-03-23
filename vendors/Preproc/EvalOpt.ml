(* Parsing command-line options *)

(* The type [options] gathers the command-line options. *)

type language = PascaLIGO | CameLIGO | ReasonLIGO

type options = <
  input   : string;
  libs    : string list;
  lang    : language;
  offsets : bool
>

let make ~input ~libs ~lang ~offsets =
  object
    method input   = input
    method libs    = libs
    method lang    = lang
    method offsets = offsets
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

let help () =
  let file = Filename.basename Sys.argv.(0) in
  printf "Usage: %s [<option> ...] <input>\n" file;
  printf "where <input> is the source file,\n";
  print "and each <option> (if any) is one of the following:";
  print "  -I <paths>             Library paths (colon-separated)";
  print "             --columns   Columns for source locations";
  print "  -h,        --help      This help";
  exit 0

(* Specifying the command-line options a la GNU *)

let input    = ref None
and libs     = ref []
and lang     = ref None
and columns  = ref false

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let specs =
  let open!Getopt in [
    'I',     nolong,    None, Some add_path;
    'h',     "help",    Some help, None;
    noshort, "columns", set columns true, None
  ]

(* Handler of anonymous arguments *)

let anonymous arg =
  match !input with
    None ->
      (match Filename.extension arg with
         ".ligo"   -> lang := Some PascaLIGO
       | ".mligo"  -> lang := Some CameLIGO
       | ".religo" -> lang := Some ReasonLIGO
       | _ -> abort (sprintf "Wrong file extension."));
      input := Some arg
  | Some _ -> abort (sprintf "Multiple inputs.")

(* Checking options and exporting them as non-mutable values *)

let check () =
  let libs = !libs

  and offsets = not !columns

  and lang =
    match !lang with
      Some lang -> lang
    | None -> assert false

  and input =
    match !input with
      Some file -> file
    | None -> abort "Missing input file."

 in make ~input ~libs ~lang ~offsets

(* Parsing the command-line options *)

let read () =
  try
    Getopt.parse_cmdline specs anonymous;
    check ()
  with Getopt.Error msg -> abort msg
