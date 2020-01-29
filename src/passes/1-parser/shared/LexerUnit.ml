(* Functor to build a standalone LIGO lexer *)

module Region = Simple_utils.Region

module type IO =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module Make (IO: IO) (Lexer: Lexer.S) =
  struct
    open Printf
    module SSet = Utils.String.Set

    (* Error printing and exception tracing *)

    let () = Printexc.record_backtrace true

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

    (* Running the lexer on the input file *)

    let scan () : (Lexer.token list, string Region.reg) Stdlib.result =
      (* Preprocessing the input *)

      if SSet.mem "cpp" IO.options#verbose
      then eprintf "%s\n%!" cpp_cmd
      else ();

      if Sys.command cpp_cmd <> 0 then
        let msg =
          sprintf "External error: the command \"%s\" failed." cpp_cmd
        in Stdlib.Error (Region.wrap_ghost msg)
      else
        match Lexer.open_token_stream (Lexer.File pp_input) with
          Ok Lexer.{read; buffer; close; _} ->
            let close_all () = close (); close_out stdout in
            let rec read_tokens tokens =
              match read ~log:(fun _ _ -> ()) buffer with
                token ->
                  if   Lexer.Token.is_eof token
                  then Stdlib.Ok (List.rev tokens)
                  else read_tokens (token::tokens)
              | exception Lexer.Error error ->
                  let file =
                    match IO.options#input with
                      None | Some "-" -> false
                    |         Some _  -> true in
                  let msg =
                    Lexer.format_error ~offsets:IO.options#offsets
                                       IO.options#mode ~file error
                  in Stdlib.Error msg in
            let result = read_tokens []
            in close_all (); result
        | Stdlib.Error (Lexer.File_opening msg) ->
            close_out stdout; Stdlib.Error (Region.wrap_ghost msg)

    (* Tracing the lexing (effectful) *)

    module Log = LexerLog.Make (Lexer)

    let trace () : (unit, string Region.reg) Stdlib.result =
      (* Preprocessing the input *)

      if SSet.mem "cpp" IO.options#verbose
      then eprintf "%s\n%!" cpp_cmd
      else ();

      if Sys.command cpp_cmd <> 0 then
        let msg =
          sprintf "External error: the command \"%s\" failed." cpp_cmd
        in Stdlib.Error (Region.wrap_ghost msg)
      else
        Log.trace ~offsets:IO.options#offsets
                  IO.options#mode
                  (Some pp_input)
                  IO.options#cmd

  end
