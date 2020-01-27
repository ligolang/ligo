module SSet = Utils.String.Set

module type IO =
  sig
    val ext : string
    val options : EvalOpt.options
  end

let parse_file generic_error
               (module Unit : ParserUnit.S)
               (parse: unit -> (Unit.Parser.ast, string Region.reg) Stdlib.result)
    : (Unit.Parser.ast, string Region.reg) Stdlib.result =
  let lib_path =
    match Unit.IO.options#libs with
        [] -> ""
    | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
             in List.fold_right mk_I libs "" in
  let prefix =
    match Unit.IO.options#input with
      None | Some "-" -> "temp"
    | Some file -> Filename.(remove_extension @@ basename file) in
  let suffix = ".pp" ^ Unit.IO.ext in
  let pp_input =
    if   SSet.mem "cpp" Unit.IO.options#verbose
    then prefix ^ suffix
    else let pp_input, pp_out =
           Filename.open_temp_file prefix suffix
         in close_out pp_out; pp_input in
  let cpp_cmd =
    match Unit.IO.options#input with
      None | Some "-" ->
        Printf.sprintf "cpp -traditional-cpp%s - > %s"
                       lib_path pp_input
    | Some file ->
        Printf.sprintf "cpp -traditional-cpp%s %s > %s"
                       lib_path file pp_input in
  let open Trace in
  let%bind () = sys_command cpp_cmd in
  match Unit.Lexer.(open_token_stream (File pp_input)) with
    Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_contract
      in parse (module Unit.IO : IO) thunk
  | Stdlib.Error (Unit.Lexer.File_opening msg) ->
      Trace.fail @@ generic_error @@ Region.wrap_ghost msg

let parse_string generic_error
                 (module Unit : ParserUnit.S) parse (s: string) =
  match Unit.Lexer.(open_token_stream (String s)) with
    Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_contract
      in parse (module Unit.IO : IO) thunk
  | Stdlib.Error (Unit.Lexer.File_opening msg) ->
      Trace.fail @@ generic_error @@ Region.wrap_ghost msg

let parse_expression generic_error
                     (module Unit : ParserUnit.S) parse (s: string)  =
  match Unit.Lexer.(open_token_stream (String s)) with
    Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_expr
      in parse (module Unit.IO : IO) thunk
  | Stdlib.Error (Unit.Lexer.File_opening msg) ->
      Trace.fail @@ generic_error @@ Region.wrap_ghost msg
