open Simple_utils
open Compiler_options
open Ligo_compile
module Constants = Commands.Constants

let doc (raw_options : Raw_options.t) directory doc_args ()
    : (string * string, string * string) result
  =
  let doc_args = String.split ~on:' ' doc_args in
  let directory =
    if Filename.is_absolute directory
    then directory
    else FilePath.make_absolute (Caml.Sys.getcwd ()) directory
  in
  let tmp_dir = Filename.temp_dir_name ^/ "ligo-doc" in
  let result =
    Trace.to_stdlib_result
    @@ fun ~raise ->
    match Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) None with
    | CameLIGO -> Format.eprintf "ligo doc is not implemented yet for CameLIGO."
    | JsLIGO ->
      (* It would be convenient to check for typedoc installation
        before generating ts files. *)
      (match Commands.does_command_exist Constants.typedoc with
      | Ok true -> ()
      | Ok false | Error _ -> raise.error `Main_typedoc_doesnt_exist);
      let syntax = Syntax_types.JsLIGO in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let files = Api_helpers.list_directory ~syntax directory in
      FileUtil.cp ~recurse:true [ directory ] tmp_dir;
      let ts_files =
        List.map files ~f:(fun source_file ->
            let typed =
              Build.qualified_typed
                ~raise
                ~options
                (Build.Source_input.From_file source_file)
            in
            let path_to_ts = FilePath.replace_extension source_file "ts" in
            let path_to_ts = FilePath.make_relative directory path_to_ts in
            let path_to_ts = FilePath.make_absolute tmp_dir path_to_ts in
            let fmt = Format.formatter_of_out_channel @@ Out_channel.create path_to_ts in
            Format.fprintf
              fmt
              "%a"
              (Type_doc.to_typescript
                 ~display_format:Display.Human_readable
                 ~no_colour:true)
              typed;
            Format.pp_print_flush fmt ();
            path_to_ts)
      in
      let doc_result =
        Commands.run_command
          ~cwd:tmp_dir
          (Constants.typedoc_generate
             ~arguments:
               ("--skipErrorChecking"
               :: "--out"
               :: (Caml.Sys.getcwd () ^/ "docs")
               :: doc_args)
             ~files:ts_files)
      in
      (match doc_result with
      | Ok () -> ()
      | Error err -> raise.error (`Main_typedoc_failed err))
  in
  FileUtil.rm ~recurse:true [ tmp_dir ];
  match result with
  | Ok ((), _) -> Ok ("", "")
  | Error (e, _) ->
    Error
      ( Format.asprintf
          "%a"
          (Main_errors.Formatter.error_ppformat
             ~display_format:Human_readable
             ~no_colour:true)
          e
      , "" )
