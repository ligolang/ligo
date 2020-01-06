(* Functor to build a standalone LIGO lexer *)

module type S =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module Make (IO: S) (Lexer: Lexer.S) =
  struct
    open Printf
    (* Error printing and exception tracing *)

    let () = Printexc.record_backtrace true

    let external_ text =
      Utils.highlight (sprintf "External error: %s" text); exit 1

    (*  Preprocessing the input source and opening the input channels *)

    (* Path for CPP inclusions (#include) *)

    let lib_path =
      match IO.options#libs with
        [] -> ""
      | libs -> let mk_I dir path = sprintf " -I %s%s" dir path
               in List.fold_right mk_I libs ""

    let prefix =
      match IO.options#input with
        None | Some "-" -> "temp"
        | Some file ->  Filename.(file |> basename |> remove_extension)

    let suffix = ".pp" ^ IO.ext

    let pp_input =
      if Utils.String.Set.mem "cpp" IO.options#verbose
      then prefix ^ suffix
      else let pp_input, pp_out = Filename.open_temp_file prefix suffix
           in close_out pp_out; pp_input

    let cpp_cmd =
      match IO.options#input with
        None | Some "-" ->
           sprintf "cpp -traditional-cpp%s - > %s"
                          lib_path pp_input
        | Some file ->
           sprintf "cpp -traditional-cpp%s %s > %s"
                          lib_path file pp_input

    let () =
      if Utils.String.Set.mem "cpp" IO.options#verbose
      then eprintf "%s\n%!" cpp_cmd;
      if Sys.command cpp_cmd <> 0 then
        external_ (sprintf "the command \"%s\" failed." cpp_cmd)

    (* Running the lexer on the input file *)

    module Log = LexerLog.Make (Lexer)

    let () = Log.trace ~offsets:IO.options#offsets
                       IO.options#mode (Some pp_input)
                       IO.options#cmd

  end
