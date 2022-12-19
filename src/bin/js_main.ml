open Js_of_ocaml
module Api = Ligo_api
module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let main source syntax =
  let entry_point = "main" in
  let views = Default_options.views in
  let syntax_v =
    match Syntax.of_ext_opt (Some syntax) with
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
      ~disable_michelson_typechecking:false
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
  match
    Api.Compile.contract
      raw_options
      (Api.Compile.Text (source, syntax_v))
      display_format
      michelson_format
      michelson_comments
      ()
  with
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
    "compile"
    (object%js
       method main code syntax =
         let code = Js.to_string code in
         let syntax = Js.to_string syntax in
         let michelson = main code syntax in
         Js.string michelson
    end)
