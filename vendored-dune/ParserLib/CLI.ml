(* Parsing command-line options *)

(* Vendor dependencies *)

module Argv   = Simple_utils.Argv
module Getopt = GetoptLib.Getopt

(* Parser parameters *)

module type PARAMETERS =
  sig
    module Config  : Preprocessor.Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* Parsing the command line options *)

module Make (LexerParams: LexerLib.CLI.PARAMETERS) : PARAMETERS =
  struct
    (* Auxiliary functions *)

    let sprintf = Printf.sprintf

    (* Help (exported) *)

    let make_help buffer : Buffer.t =
      let options = [
        "      --mono           Use Menhir monolithic API";
        "Pretty-printing:";
        "      --pretty         Pretty-print the input";
        "      --width=<n>      Width for --pretty";
        "CST printing:";
        "      --cst            Print the CST";
        "      --no-layout      With --cst, do not print the tree layout";
        "      --no-regions     With --cst, do not print the source regions";
        "Error recovery:";
        "      --recovery       Enable error recovery";
        "Debugging:";
        "      --used-tokens    Print the tokens up to the syntax error";
        "      --trace-recovery[=<file>]";
        "                       Enable verbose printing of intermediate steps\n                       of the error recovery algorithm to output_file\n                       if provided, or stdout otherwise"
      ] in
      begin
        Buffer.add_string buffer (String.concat ~sep:"\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Specifying the command-line options a la GNU *)

    (* Monolithic API and error recovery *)

    let mono     = ref false
    and recovery = ref false

    (* Pretty-printing options *)

    and pretty = ref false
    and width  = ref (None : int option)

    (* CST printing options *)

    and cst     = ref false
    and layout  = ref true
    and regions = ref true

    (* Debug options *)

    and used_tokens    = ref false
    and trace_recovery = ref (None : string option option)

    (* Others *)

    and help    = ref false
    and version = ref false
    and cli     = ref false

    (* --trace_recovery *)

    let print_trace_recovery = function
      None -> "None"
    | Some None -> "Some None"
    | Some Some path -> "Some (Some " ^ path ^ ")"

    let set_trace_recovery path =
      if Caml.(!trace_recovery = None)
      then trace_recovery := Some (Some path)
      else raise (Getopt.Error
                    "Only one --trace-recovery option allowed.")

    (* --width=<arg> *)

    let set_width (arg : string) =
      if Caml.(!width = None)
      then match Base.int_of_string_opt arg with
             None -> raise (Getopt.Error "Invalid width.")
           | Some n -> width := Some n
      else raise (Getopt.Error "Only one --width option allowed.")

    let print_width = function
      None -> "None"
    | Some n -> string_of_int n

    (* Specifying the command-line options a la GNU

       See [GetoptLib.Getopt] for the layout of the command line and
       the specification of the options. *)

    let specs =
      Getopt.[
        noshort, "mono",           set mono true, None;
        noshort, "pretty",         set pretty true, None;
        noshort, "width",          None, Some set_width;
        noshort, "cst",            set cst true, None;
        noshort, "no-layout",      set layout false, None;
        noshort, "no-regions",     set regions false, None;
        noshort, "recovery",       set recovery true, None;
        noshort, "trace-recovery", set trace_recovery (Some None),
                                   Some set_trace_recovery;
        noshort, "used-tokens",    set used_tokens true, None;

        noshort, "cli",            set cli true, None;
        'h',     "help",           set help true, None;
        'v',     "version",        set version true, None
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

    module SSet = Argv.SSet

    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--mono"
      |> add "--pretty"
      |> add "--cst"
      |> add "--recovery"
      |> add "--trace-recovery"
      |> add "--used-tokens"
      |> add "--no-layout"
      |> add "--no-regions"

      (* The following options are present in all CLI *)

      |> add "--cli"
      |> add "--help"    |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open SSet in
      empty
      |> add "--trace-recovery"
      |> add "--width"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      LexerParams.Status.t
    | `DependsOn of string * string
    ]

    let status = (LexerParams.Status.status :> status)

    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let mono     = !mono
    and pretty   = !pretty
    and width    = !width
    and cst      = !cst
    and layout   = !layout
    and regions  = !regions
    and recovery = !recovery

    (* Debug options *)

    and used_tokens    = !used_tokens
    and trace_recovery = !trace_recovery

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli buffer : Buffer.t =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        sprintf "mono           = %b" mono;
        sprintf "pretty         = %b" pretty;
        sprintf "width          = %s" (print_width width);
        sprintf "cst            = %b" cst;
        sprintf "layout         = %b" layout;
        sprintf "regions        = %b" regions;
        sprintf "recovery       = %b" recovery;
        sprintf "used_tokens    = %b" used_tokens;
        sprintf "trace_recovery = %s" (print_trace_recovery trace_recovery)]
      in
      begin
        Buffer.add_string buffer (String.concat ~sep:"\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* STATUS *)

    (* Checking combinations of options *)

    let status =
      match
        mono, pretty,  cst, recovery, trace_recovery with
      |    _,  true,  true,        _,     _ -> `Conflict ("--pretty", "--cst")
      | true,     _,     _,     true,     _ -> `Conflict ("--mono", "--recovery")
      |    _,     _,     _,    false,  Some _ -> `DependsOn ("--trace-recovery", "--recovery")
      |    _,     _,     _,        _,     _ -> status

    let status =
      match status with
        `Help buffer  -> `Help (make_help buffer)
      | `CLI buffer   -> `CLI (make_cli buffer)
      | `Version _    -> `Version Version.version
      | _             -> status

    (* Packaging *)

    module Config = LexerParams.Config

    module Options =
      struct
        include LexerParams.Options
        let mono           = mono
        let pretty         = pretty
        let width          = width
        let cst            = cst
        let recovery       = recovery
        let trace_recovery = trace_recovery
        let used_tokens    = used_tokens
        let layout         = layout
        let regions        = regions
      end

    module Status =
      struct
        type t = status
        type nonrec status = status
        let status = status
      end
  end


(* Default parameters (without actually reading the CLI) *)

module MakeDefault (LexerParams: LexerLib.CLI.PARAMETERS) =
  struct
    module Config  = LexerParams.Config
    module Options = Options.MakeDefault (LexerParams.Options)

    module Status =
      struct
        type t = Status.t
        type status = t
        let status = `Done
      end
  end
