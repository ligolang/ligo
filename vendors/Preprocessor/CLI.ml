(* Parsing the command-line options *)

(* Vendor dependencies *)

module Argv = Simple_utils.Argv

(* The signature [COMMENTS] specifies the kind of comments
   expected. Those fields do not correspond to CLI (command-line
   options), as they are for internal use. *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

module type MODULES =
  sig
    val mk_module : string -> string -> string
  end
(* Command-Line Interface (CLI) options *)

module type S =
  sig
    include COMMENTS
    include MODULES

    val input        : string option (* input file     *)
    val extension    : string option (* file extension *)
    val dirs         : string list   (* -I             *)
    val project_root : string option (* --project-root *)
    val show_pp      : bool          (* --show-pp      *)
    val offsets      : bool          (* neg --columns  *)

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
  end

(* Parsing the command line options *)

module Make (Comments: COMMENTS) (Modules : MODULES): S =
  struct
    include Comments
    include Modules

    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    let split_at_colon = Str.(split (regexp ":"))

    let add_path dirs path = dirs := !dirs @ split_at_colon path

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
        "  -h, --help         This help";
        "  -v, --version      Commit hash on stdout";
        "      --cli          Print given options (debug)";
        "      --columns      Columns for source locations";
        "      --show-pp      Print result of preprocessing";
        "      --project-root Path to the root of the project"
      ] in
      begin
        Buffer.add_string buffer header;
        Buffer.add_string buffer (String.concat "\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    let input        = ref None
    and dirs         = ref []

    and project_root = ref None
    and columns      = ref false
    and show_pp      = ref false

    and help         = ref false
    and version      = ref false
    and cli          = ref false

    (* Specifying the command-line options a la GNU

       See [Getopt] for the layout of the command line and
       the specification of the options. *)

    let specs =
      let open Getopt in [
        'I',     nolong,         None,             Some (add_path dirs);
        noshort, "columns",      set columns true, None;
        noshort, "show-pp",      set show_pp true, None;

        noshort, "cli",          set cli true,     None;
        'h',     "help",         set help true,    None;
        'v',     "version",      set version true, None;
        noshort, "project-root", None,             Some (set_path project_root);
      ]

    (* Handler of anonymous arguments.

       The test on [arg] is for the hack below not to trigger the
       error "Multiple inputs" for erased options. *)

    let anonymous arg =
      if arg <> "" then
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
      |> add "--columns"

      (* The following options are present in all CLI *)
      |> add "--cli"                  (* For debugging *)
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open Argv.SSet in
      empty
      |> add "-I"
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
    and project_root = !project_root
    and offsets      = not !columns
    and show_pp      = !show_pp
    and help         = !help
    and version = !version

    let string_of_opt convert = function
      None -> "None"
    | Some x -> sprintf "Some %S" (convert x)

    let string_of_dirs = sprintf "[%s]" (String.concat ";" dirs)

    (* Re-exporting and printing on stdout the CLI options *)

    let cli_buffer = Buffer.create 131

    let () =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        "CLI options";
        sprintf "input        = %s" (string_of_opt (fun x -> x) !input);
        sprintf "dirs         = %s" string_of_dirs;
        sprintf "show-pp      = %b" show_pp;
        sprintf "columns      = %b" (not offsets);
        sprintf "project-root = %s" (string_of_opt (fun x -> x) project_root)
      ] in
    begin
      Buffer.add_string cli_buffer (String.concat "\n" options);
      Buffer.add_char   cli_buffer '\n'
    end

    (* Input and status *)

    let input, status =
      match status with
        `SyntaxError _  -> !input, status
      | _ when help     -> !input, `Help (make_help ())
      | _ when !cli     -> !input, `CLI cli_buffer
      | _ when version  -> !input, `Version Version.version
      | _ -> match !input with
               None | Some "-" -> None, `Done
             | Some file_path ->
                 !input,
                 if   Sys.file_exists file_path
                 then `Done
                 else `FileNotFound "Source file not found."

    (* File extension (must come after handling of input above) *)

    let extension =
      match input with
        None -> None
      | Some file_path ->
          let x = Filename.extension file_path
          in if x = "" then None else Some x
   end
