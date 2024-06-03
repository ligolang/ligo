(* Parsing command-line options for the lexer *)

open Core

(* Vendor dependencies *)

module Argv   = Simple_utils.Argv
module Getopt = GetoptLib.Getopt

(* Lexer parameters *)

module type PARAMETERS =
  sig
    module Config  : Preprocessor.Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* Parsing the command line options *)

module Make (PreprocParams: Preprocessor.CLI.PARAMETERS) : PARAMETERS =
  struct
    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    (* Help (exported) *)

    let make_help buffer : Buffer.t =
      let options = [
        "  -t, --tokens         Print tokens";
        "  -u, --units          Print lexical units";
        "  -c, --copy           Print lexemes and markup";
        "      --bytes          Bytes for source locations";
        "      --preprocess     Run the preprocessor";
        "      --post=<pass>    Run postprocessing up to pass <pass> \n\
        \                       (0/1/2/etc.). None: 0. Default: all";
        "      --print-passes   Print the self-passes's names when applied";
        "      --string=<str>   Use a string as input. Discard any other input.";
        "      --jsligo[=<dst>] Transpile to JsLIGO. Default: stdout."
      ] in
      begin
        Buffer.add_string buffer (Base.String.concat ~sep:"\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Global references for the CLI options *)

    let copy         = ref false
    and tokens       = ref false
    and units        = ref false
    and bytes        = ref false
    and preprocess   = ref false
    and string       = ref (None : string option)
    and post         = ref (None : int option)
    and print_passes = ref false
    and jsligo       = ref (None : string option option)

    and help    = ref false
    and version = ref false
    and cli     = ref false

    (* --jsligo *)

    let print_jsligo = function
      None -> "None"
    | Some None -> "Some None"
    | Some Some file -> "Some (Some " ^ file ^ ")"

    let set_jsligo file =
      match !jsligo with
        None -> jsligo := Some (Some file)
      | _ -> raise (Getopt.Error "Only one --jsligo option allowed.")

    (* --post *)

    let print_post = function
      None   -> "None (all passes)"
    | Some n -> string_of_int n

    let set_post arg =
      match !post with
        Some _ -> raise (Getopt.Error "Only one --post option allowed.")
      | None ->
          match Stdlib.int_of_string_opt arg with
            None -> raise (Getopt.Error "Invalid pass number.")
          | Some num when num < 0 ->
              raise (Getopt.Error "Invalid pass number.")
          | passes -> post := passes

    (* --string *)

    let print_string = function
      None -> "None"
    | Some s -> Printf.sprintf "Some %S" s

    let set_string str =
      match !string with
        None ->  string := Some str
      | _ -> raise (Getopt.Error "Only one --string option allowed.")

    (* Specifying the command-line options a la GNU

       See [GetoptLib.Getopt] for the layout of the command line and
       the specification of the options. *)

    let specs =
      Getopt.[
        'c',     "copy",         set copy true, None;
        't',     "tokens",       set tokens true, None;
        'u',     "units",        set units true, None;
        noshort, "bytes",        set bytes true, None;
        noshort, "preprocess",   set preprocess true, None;
        noshort, "string",       None, Some set_string;
        noshort, "post",         None, Some set_post;
        noshort, "print-passes", set print_passes true, None;
        noshort, "jsligo",       set jsligo (Some None),
                                 Some set_jsligo;
        noshort, "cli",          set cli true, None;
        'h',     "help",         set help true, None;
        'v',     "version",      set version true, None
      ]

     (* Handler of anonymous arguments: those have been handled by a
        previous IO *)

    let anonymous _arg = ()

    (* Parsing the command-line options *)

    (* We do not want the exception [Getopt.Error] to be raised when
       finding an unknown option.

       The following is a hack to filter out unknown options but
       leaving correct ones, even if their syntax is invalid (this
       will result in an error in [Getopt.parse_cmdline] below. Also,
       we assume that there are no concatenated short options (here,
       the only possible combinations are "-hv" and "-vh") and that
       anonymous arguments (here, a unique text file) is given after
       "--".

       We make a copy of [Sys.argv], we filter it in a list, the
       resulting list is copied to [Sys.argv] (with the remaning cells
       set to [""]), we parse the options with [Getopt.parse_cmdline]
       and we finally restore [Sys.argv] from its original copy. *)

    let add string set = Set.add set string

    let opt_wo_arg =
      String.Set.empty
      |> add "--copy"   |> add "-c"
      |> add "--tokens" |> add "-t"
      |> add "--units"  |> add "-u"
      |> add "--bytes"
      |> add "--preprocess"
      |> add "--print-passes"
      |> add "--jsligo"

      (* The following options are present in all CLIs *)
      |> add "--cli"
      |> add "--help"    |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      String.Set.empty
      |> add "--post"
      |> add "--jsligo"
      |> add "--string"

    let argv_copy = Array.copy (Sys.get_argv ())

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      PreprocParams.Status.t
    | `Conflict of string * string
    ]

    let status = (PreprocParams.Status.status :> status)

    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      let argv = Sys.get_argv () in
      for i = 0 to Array.length argv - 1 do
        argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let copy         = !copy
    and tokens       = !tokens
    and units        = !units
    and mode         = if !bytes then `Byte else `Point
    and preprocess   = !preprocess
    and postprocess  = !post
    and print_passes = !print_passes
    and jsligo       = !jsligo
    and string       = !string

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli buffer : Buffer.t =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        sprintf "copy         = %b" copy;
        sprintf "tokens       = %b" tokens;
        sprintf "units        = %b" units;
        sprintf "bytes        = %b" !bytes;
        sprintf "preprocess   = %b" preprocess;
        sprintf "post         = %s" (print_post postprocess);
        sprintf "print_passes = %b" print_passes;
        sprintf "jsligo       = %s" (print_jsligo jsligo);
        sprintf "string       = %S" (print_string string)] in
    begin
      Buffer.add_string buffer (Base.String.concat ~sep:"\n" options);
      Buffer.add_char buffer '\n';
      buffer
    end

    (* STATUS *)

    (* Checking combinations of options *)

    let status, command =
      match copy, units, tokens with
        true,  false, false -> status, Some `Copy
      | false, true,  false -> status, Some `Units
      | false, false, true  -> status, Some `Tokens
      | false, false, false -> status, None
      | true,  true,  _     -> `Conflict ("--copy", "--units"), None
      | true,     _,  true  -> `Conflict ("--copy", "--tokens"), None
      | _,     true,  true  -> `Conflict ("--units", "--tokens"), None

    let status =
      match status with
        `Help buffer -> `Help (make_help buffer)
      | `CLI buffer  -> `CLI (make_cli buffer)
      | `Version _   -> `Version Version.version
      | _            -> status

    (* Packaging *)

    module Config = PreprocParams.Config

    module Options =
      struct
        include PreprocParams.Options

        let postprocess  = postprocess
        let preprocess   = preprocess
        let mode         = mode
        let command      = command
        let string       = string
        let print_passes = print_passes
        let jsligo       = jsligo
      end

    module Status =
      struct
        type t = status
        type nonrec status = status
        let status = status
      end
  end

(* Default parameters (without actually reading the CLI) *)

module MakeDefault (PreprocParams : Preprocessor.CLI.PARAMETERS) =
  struct
    module Config  = PreprocParams.Config
    module Options = Options.MakeDefault (PreprocParams.Options)

    module Status =
      struct
        type t = Status.t
        type status = t
        let status = `Done
      end
  end
