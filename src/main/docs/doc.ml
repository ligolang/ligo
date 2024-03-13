open Simple_utils
open Compiler_options
open Ligo_compile
module Constants = Commands.Constants

let html_using_type_doc (raw_options : Raw_options.t) directory doc_args ()
    : (unit * Main_warnings.all list, Main_errors.all * Main_warnings.all list) result
  =
  let doc_args = String.split ~on:' ' doc_args in
  let directory =
    if Filename.is_absolute directory
    then directory
    else FilePath.make_absolute (Caml.Sys.getcwd ()) directory
  in
  let tmp_dir = Filename.temp_dir_name ^/ "ligo-doc" in
  let cleanup () = FileUtil.rm ~recurse:true [ tmp_dir ] in
  Docs_utils.with_raise ~cleanup
  @@ fun ~raise ->
  match Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) None with
  | CameLIGO -> Format.eprintf "ligo doc --type-doc supports only JsLIGO."
  | JsLIGO ->
    (* It would be convenient to check for typedoc installation
        before generating ts files. *)
    (match Commands.does_command_exist Constants.typedoc with
    | Ok true -> ()
    | Ok false | Error _ ->
      raise.error @@ `Main_external_doc_tool_doesnt_exist (Constants.typedoc, `Npm));
    let syntax = Syntax_types.JsLIGO in
    let options =
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      Compiler_options.make ~protocol_version ~raw_options ~syntax ()
    in
    let files = Ligo_api.Api_helpers.list_directory ~syntax directory in
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
    let output_dir = Caml.Sys.getcwd () ^/ "docs" in
    let doc_result =
      Commands.run_command
        ~cwd:tmp_dir
        (Constants.typedoc_generate
           ~arguments:("--skipErrorChecking" :: "--out" :: output_dir :: doc_args)
           ~files:ts_files)
    in
    (match doc_result with
    | Ok () -> ()
    | Error err -> raise.error (`Main_external_doc_tool_failed (Constants.typedoc, err)))


let markdown_doc
    (case : [ `Ast_typed ] * [ `Mdx ])
    (raw_options : Raw_options.t)
    directory
    ?output_directory
    doc_args
    ()
    : (unit * Main_warnings.all list, Main_errors.all * Main_warnings.all list) result
  =
  ignore doc_args;
  let to_normalised_abs_path path =
    let path =
      if Filename.is_absolute path
      then path
      else FilePath.make_absolute (Caml.Sys.getcwd ()) path
    in
    FileUtil.mkdir ~parent:true path;
    (* [Filename_unix.realpath] works only with existing paths *)
    Filename_unix.realpath path
  in
  let directory = to_normalised_abs_path directory in
  let output_directory =
    Option.value_map
      ~default:(Filename.concat directory "markdown_docs")
      ~f:to_normalised_abs_path
      output_directory
  in
  Docs_utils.with_raise
  @@ fun ~raise ->
  let options =
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make ~protocol_version ~raw_options ()
  in
  let files = Ligo_api.Api_helpers.list_directory directory in
  if List.is_empty files then failwith "ligo doc: no ligo files found in input directory";
  let _md_files =
    List.map files ~f:(fun source_file ->
        let source_syntax =
          Option.value_exn (* list_directory returns only LIGO files *)
          @@ Syntax.of_ext_opt
          @@ snd
          @@ Filename.split_extension source_file
        in
        match case with
        | `Ast_typed, markdown_dialect ->
          let typed =
            Build.qualified_typed
              ~raise
              ~options
              (Build.Source_input.From_file source_file)
          in
          (match markdown_dialect with
          | `Mdx ->
            (* We create a folder for each source file *)
            let rel_path = FilePath.make_relative directory source_file in
            let folder = FilePath.make_absolute output_directory rel_path in
            FileUtil.mkdir ~parent:true folder;
            let files =
              Mdx.to_mdx ~source_syntax ~source_file ?file_name:(Some rel_path) typed
            in
            List.map files ~f:(fun { file_name; contents } ->
                let abs_path = FilePath.concat folder (String.lowercase file_name) in
                Out_channel.write_all abs_path ~data:contents;
                abs_path)))
  in
  (* printf "%s%s" "Created .md files:\n" (String.concat ~sep:"\n" md_files); *)
  ()


let doc ~mdx ~type_doc ?output_directory raw_options directory doc_args () =
  Docs_utils.format_errors
  @@
  if mdx
  then markdown_doc (`Ast_typed, `Mdx) ?output_directory raw_options directory doc_args ()
  else if type_doc
  then html_using_type_doc raw_options directory doc_args ()
  else Error (`Main_ligo_doc_cli_error "please use either --mdx or --type-doc", [])
