(* Command-Line Interface (CLI) *)

(* Vendor dependencies *)

module Argv   = Simple_utils.Argv  (* Filters the command line *)
module Getopt = GetoptLib.Getopt   (* GNU Getopt *)

(* Configuration, options and the parsing status of the latter *)

module type PARAMETERS =
  sig
    module Config  : Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* Parsing the command line options *)

module Make (Config : Config.S) : PARAMETERS =
  struct
    (* General configuration *)

    module Config = Config

    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    let split_at_colon = Str.(split (regexp ":"))

    let add_elem elems list = elems := !elems @ split_at_colon list

    let add_path = add_elem

    let add_def = add_elem

    let set_path project_root path = project_root := Some path

    let make_help () : Buffer.t =
      let file   = Filename.basename Sys.argv.(0) in
      let buffer = Buffer.create 203 in
      let header =
        sprintf "Usage: %s [<option> ...] -- [<input>]\n\
                 where <input> is the source file (default: stdin),\n\
                 and each <option> (if any) is one of the following:\n"
                file
      and options = [
        "  -I <paths>         Inclusion paths (colon-separated)";
        "  -D <symbols>       Predefined symbols (colon-separated)";
        "  -h, --help         This help";
        "  -v, --version      Commit hash on stdout";
        "      --cli          Print given options (debug)";
        "      --columns      Columns for source locations";
        "      --show-pp      Print result of preprocessing";
        "      --no-colour    Disable coloured printing on stdout";
        "      --project-root Path to the root of the project"
      ] in
      begin
        Buffer.add_string buffer header;
        Buffer.add_string buffer (String.concat ~sep:"\n" options);
        Buffer.add_char buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    let input        = ref None
    and dirs         = ref []
    and define       = ref []

    and project_root = ref None
    and columns      = ref false
    and show_pp      = ref false
    and no_colour    = ref false

    and help         = ref false
    and version      = ref false
    and cli          = ref false

    (* Specifying the command-line options a la GNU

       See [GetoptLib.Getopt] for the layout of the command line and
       the specification of the options. *)

    let specs =
      Getopt.[
        'I',     nolong,         None,             Some (add_path dirs);
        'D',     nolong,         None,             Some (add_def define);
        noshort, "columns",      set columns true, None;
        noshort, "show-pp",      set show_pp true, None;
        noshort, "no-colour",    set no_colour true, None;

        noshort, "cli",          set cli true,     None;
        'h',     "help",         set help true,    None;
        'v',     "version",      set version true, None;
        noshort, "project-root", None, Some (set_path project_root)
      ]

    (* Handler of anonymous arguments.

       The test on [arg] is for the hack below not to trigger the
       error "Multiple inputs" for erased options. *)

    let anonymous arg =
      if String.(arg <> "") then
        match !input with
          None -> input := Some arg
        | Some _ -> raise (Getopt.Error "Multiple inputs.")

    (* Parsing the command-line options *)

    (* We do not want the exception [Getopt.Error] to be raised when
       finding an unknown option.

         The following is a hack to filter out unknown options but
       leaving correct ones, even if their syntax is invalid and will
       result in an exception [Getopt.Error] raised by
       [Getopt.parse_cmdline] below.

         IMPORTANT: We assume that there are no concatenated short
       options (here, the only possible combinations are "-hv" and
       "-vh") and that anonymous arguments (here, a unique text file)
       is given after "--".

         First, we make a backup copy of [Sys.argv]. Second, we filter
       it in a list by calling [Argv.filter]. That function performs a
       side effect on [Sys.argv]: unknown options are removed and
       compacted. That is why [Sys.argv] has to be restored from the
       backup after parsing the command-line: another parse is now
       possible by another client. *)

    module SSet = Argv.SSet

    let opt_wo_arg =
      let open Argv.SSet in
      empty
      |> add "--show-pp"
      |> add "--no-colour"
      |> add "--columns"

      (* The following options are present in all CLI *)
      |> add "--cli"                  (* For debugging *)
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open Argv.SSet in
      empty
      |> add "-I"
      |> add "-D"
      |> add "--project-root"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    | `WrongFileExt of string
    ]

    let status =
      try
        Getopt.parse_cmdline specs anonymous;
        `Done (* Default. Other values assigned below. *)
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let dirs         = !dirs
    and define       = !define
    and project_root = !project_root
    and offsets      = not !columns
    and show_pp      = !show_pp
    and no_colour    = !no_colour
    and help         = !help
    and version      = !version

    (* Optional arguments *)

    let string_of_opt convert = function
      None   -> "None"
    | Some x -> sprintf "Some %S" (convert x)

    let string_of_input        = string_of_opt (fun x -> x) !input
    let string_of_project_root = string_of_opt (fun x -> x) project_root

    (* Lists *)

    let string_of_list list = sprintf "[%s]" (String.concat ~sep:";" list)

    let string_of_dirs   = string_of_list dirs
    let string_of_define = string_of_list define

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli () =
      let buffer = Buffer.create 131 in
      (* Options "help", "version" and "cli" are not given.
         Do not change the spacing before "=" *)
      let options = [
        sprintf "input        = %s" string_of_input;
        sprintf "dirs         = %s" string_of_dirs;
        sprintf "define       = %s" string_of_define;
        sprintf "show-pp      = %b" show_pp;
        sprintf "no-colour    = %b" no_colour;
        sprintf "columns      = %b" (not offsets);
        sprintf "project-root = %s" string_of_project_root
      ] in
    begin
      Buffer.add_string buffer (String.concat ~sep:"\n" options);
      Buffer.add_char   buffer '\n';
      buffer
    end

    (* Checking the input file (if any) *)

    let check_file file_path =
      let actual = Caml.Filename.extension file_path in
      match Config.file_ext with
        Some expected when String.(expected <> actual) ->
          let msg = sprintf "Expected file extension %S." expected
          in `WrongFileExt msg
      | Some _ | None ->
          if   Caml.Sys.file_exists file_path
          then `Done
          else `FileNotFound "Source file not found."

    (* Input and status *)

    (* At this point, [status] can either be [`Done] or
       [`SyntaxError]. See assignment to [status] above. *)

    let input, status =
      match status with
        `SyntaxError _     -> !input, status
      | `Done when help    -> !input, `Help (make_help ())
      | `Done when !cli    -> !input, `CLI (make_cli ())
      | `Done when version -> !input, `Version Version.version
      | `Done -> match !input with
                   None | Some "-" -> None, `Done
                 | Some file_path -> !input, check_file file_path

    (* Packaging the CLI options *)

    module Options =
      struct
        let input        = input
        and dirs         = dirs
        and define       = define
        and project_root = project_root
        and show_pp      = show_pp
        and no_colour    = no_colour
        and offsets      = offsets
      end

    (* Packaging the parsing status *)

    module Status =
      struct
        type t = status
        type status = t
        let status = status
      end
   end

(* Default parameters (without actually reading the CLI) *)

module MakeDefault (Config : Config.S) =
  struct
    module Config  = Config
    module Options = Options.Default

    module Status =
      struct
        type t = Status.t
        type status = t
        let status = `Done
      end
  end
