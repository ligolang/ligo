open Js_of_ocaml
module Api = Ligo_api
module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let main source syntax =
  let entry_point = [ "main" ] in
  let views = Default_options.views in
  let syntax_v =
    match
      Syntax.of_ext_opt ~support_pascaligo:Default_options.deprecated (Some syntax)
    with
    | Some v -> v
    | None -> failwith ("Invalid syntax " ^ syntax)
  in
  let protocol_version = "lima" in
  let display_format = Simple_utils.Display.human_readable in
  let michelson_format = `Text in
  let michelson_comments = [ `Source ] in
  let raw_options =
    Raw_options.make
      ~entry_point
      ~syntax
      ~views
      ~protocol_version
      ~disable_michelson_typechecking:true
      ~experimental_disable_optimizations_for_debugging:false
      ~enable_typed_opt:false
      ~no_stdlib:false
      ~warning_as_error:false
      ~no_colour:true
      ~constants:Default_options.constants
      ~file_constants:None
      ~project_root:None
      ~warn_unused_rec:true
      ()
  in
  let value_format, f =
    Api.Compile.contract
      raw_options
      (Api.Compile.Text (source, syntax_v))
      michelson_format
      michelson_comments
  in
  let result = Simple_utils.Trace.to_stdlib_result f in
  let format =
    Simple_utils.Display.bind_format value_format Main_errors.Formatter.error_format
  in
  let value, _analytics =
    match result with
    | Ok ((v, analytics), _w) -> Ok v, analytics
    | Error (e, _w) -> Error e, []
  in
  let formatted_result =
    Ligo_api.Api_helpers.toplevel
      ~warning_as_error:false
      ~display_format
      ~no_colour:false
      (Displayable { value; format })
      result
  in
  match formatted_result with
  | Ok (a, b) ->
    print_endline a;
    print_endline b;
    a
  | Error (a, b) ->
    print_endline "error";
    print_endline a;
    print_endline b;
    "<failed>"


let _ =
  Js.export
    "ligo"
    (object%js
       method compile code syntax =
         let code = Js.to_string code in
         let syntax = Js.to_string syntax in
         let michelson = main code syntax in
         Js.string michelson
    end)
