open Cli_helpers
module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let is_dev = ref true

let create_arg_type ?key of_string =
  Core.Command.Arg_type.create ?key of_string ~complete:(fun _ ~part ->
      let completions =
        (* `compgen -f` handles some fiddly things nicely, e.g. completing "foo" and
         "foo/" appropriately. *)
        let command = sprintf "bash -c 'compgen -f %s'" part in
        let chan_in = Ligo_unix.open_process_in command in
        let completions = In_channel.input_lines chan_in in
        ignore (Ligo_unix.close_process_in chan_in);
        List.map (List.sort ~compare:String.compare completions) ~f:(fun comp ->
            match Caml.Sys.is_directory comp with
            | true -> comp ^ "/"
            | _ -> comp)
      in
      match completions with
      | [ dir ] when String.is_suffix dir ~suffix:"/" ->
        (* If the only match is a directory, we fake out bash here by creating a bogus
         entry, which the user will never see - it forces bash to push the completion
         out to the slash. Then when the user hits tab again, they will be at the end
         of the line, at the directory with a slash and completion will continue into
         the subdirectory.
      *)
        [ dir; dir ^ "x" ]
      | _ -> completions)


let entry_point =
  let open Command.Param in
  let name = "e" in
  let doc =
    "ENTRY-POINT (this command is deprecated) the entry-point that will be compiled."
  in
  let spec =
    optional_with_default []
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string
  in
  flag ~doc ~aliases:[ "--entry-point" ] name spec


let parameter_entrypoint =
  let open Command.Param in
  let name = "e" in
  let doc =
    "ENTRY-POINT the entry-point to be matched against the parameter expression"
  in
  let spec = optional string in
  flag ~doc ~aliases:[ "--entry-point" ] name spec


let function_name =
  let name = "FUNCTION" in
  Command.Param.(anon (name %: create_arg_type Fn.id))


let module_ =
  let open Command.Param in
  let name = "m" in
  let doc = "MODULE the entry-point will be compiled from that module." in
  let spec = optional_with_default Default_options.module_ string in
  flag ~doc ~aliases:[ "--module" ] name spec


let source_file =
  let name = "SOURCE_FILE" in
  Command.Param.(anon (name %: create_arg_type Fn.id))


let source_files =
  let name = "SOURCE_FILES" in
  Command.Param.(anon @@ non_empty_sequence_as_pair (name %: create_arg_type Fn.id))


let package_name =
  let name = "PACKAGE_NAME" in
  Command.Param.(anon (maybe (name %: string)))


let username =
  let open Command.Param in
  let name = "--username" in
  let doc = "Username registered with the registry" in
  let spec = required string in
  flag ~doc name spec


let named_arg_package_name =
  let open Command.Param in
  let name = "--package-name" in
  let doc = "Name of the package on which publish/unpublish is executed" in
  let spec = optional string in
  flag ~doc name spec


let named_arg_package_version =
  let open Command.Param in
  let name = "--package-version" in
  let doc = "Version of the package on which publish/unpublish is executed" in
  let spec = optional string in
  flag ~doc name spec


let expression purpose =
  let name = purpose ^ "_EXPRESSION" in
  Command.Param.(anon (name %: string))


let libraries : string list Command.Param.t =
  let open Command.Param in
  let name = "--library" in
  let doc =
    "LIBS A comma-separated list of paths to directories where to search for files to be \
     included by the preprocessor"
  in
  let spec =
    optional_with_default Default_options.libraries
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string
  in
  flag ~doc ~aliases:[ "l" ] name spec


let template =
  let open Command.Param in
  let doc =
    "TEMPLATE the template name which will be used to generate folder. You can obtain \
     available list by running ligo init list. If not provided default is empty-project."
  in
  let spec = optional_with_default "empty" string in
  flag ~doc ~aliases:[ "t" ] "--template" spec


let skip_analytics =
  let open Command.Param in
  let doc =
    "Avoid ligo analytics publication. Configurable with environment variable \
     LIGO_SKIP_ANALYTICS too"
  in
  flag ~doc "--skip-analytics" no_arg


let template_list =
  let open Command.Param in
  let name = "--template-list" in
  let doc =
    "If present, change cmmand behavior and list available templates for this command."
  in
  flag ~doc name no_arg


let project_name =
  let name = "PROJECT_NAME" in
  let _desc = "The generated project name" in
  Command.Param.(anon (maybe (name %: string)))


let syntax =
  let open Command.Param in
  let doc =
    "SYNTAX the syntax that will be used. Currently supported syntaxes are \"cameligo\" \
     and \"jsligo\". By default, the syntax is guessed from the extension (.mligo and \
     .jsligo respectively)."
  in
  let spec = optional_with_default Default_options.syntax string in
  flag ~doc ~aliases:[ "s" ] "--syntax" spec


let from_syntax =
  let open Command.Param in
  let doc = "SYNTAX the syntax to the transpilation input." in
  let spec = optional_with_default Default_options.syntax string in
  flag ~doc "--from-syntax" spec


let to_syntax =
  let open Command.Param in
  let doc = "SYNTAX the syntax to the transpilation output." in
  let spec = optional_with_default Default_options.syntax string in
  flag ~doc "--to-syntax" spec


let nanopass =
  let open Command.Param in
  let doc =
    "NANOPASS the nanopass name before/after which we stop executing the nanopasses. Use \
     NAME+ for after and NAME for before, case do not matter (only for debug prints)"
  in
  let spec = optional string in
  flag ~doc ~aliases:[ "nano" ] "--nanopass" spec


let on_chain_views : _ Command.Param.t =
  let open Command.Param in
  let doc =
    "VIEWS (this command is deprecated) A list of declaration name that will be compiled \
     as on-chain views, separated by ','"
  in
  let spec =
    optional_with_default []
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string
  in
  flag ~doc ~aliases:[ "v" ] "--views" spec


let constants : _ Command.Param.t =
  let open Command.Param in
  let doc =
    "CONSTANTS A list of global constants that will be assumed in the context, separated \
     by ','"
  in
  let spec =
    optional_with_default Default_options.constants
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string
  in
  flag ~doc ~aliases:[ "c" ] "--constants" spec


let file_constants : _ Command.Param.t =
  let open Command.Param in
  let doc =
    "FILE_CONSTANTS A file with a JSON list of strings with Michelson code. Those \
     Michelson values will be registered as global constants in the context."
  in
  let spec = optional string in
  flag ~doc "--file-constants" spec


let steps =
  let open Command.Param in
  let doc = "INT a bound in the number of steps to be done by the interpreter." in
  let spec = optional_with_default Default_options.steps int in
  flag ~doc ~aliases:[ "n" ] "--steps" spec


let protocol_version =
  let open Command.Param in
  let open Environment.Protocols in
  let plist =
    Format.asprintf
      "%a"
      (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string)
      protocols_str
  in
  let doc =
    Format.asprintf
      "PROTOCOL choose protocol's types/values pre-loaded into the LIGO environment %s. \
       By default, the current protocol (%s) will be used"
      plist
      (variant_to_string current)
  in
  let spec = optional_with_default Default_options.protocol_version string in
  flag ~doc ~aliases:[ "--protocol" ] "p" spec


let cli_expr_inj =
  let open Command.Param in
  let doc =
    "EXPRESSION an expression passed to LIGO interpreter, accessible through variable \
     'cli_arg'"
  in
  let spec = optional string in
  flag ~doc "--arg" spec


let req_syntax =
  let open Command.Param in
  let name = "SYNTAX" in
  anon (name %: string)


let init_file =
  let open Command.Param in
  let doc =
    "FILENAME the path to the smart contract file to be used for context initialization."
  in
  let spec = optional string in
  flag ~doc "--init-file" spec


let amount =
  let open Command.Param in
  let name = "--amount" in
  let doc =
    "INT the tezos amount the Michelson interpreter will use for the transaction."
  in
  let spec = optional_with_default "0" string in
  flag ~doc name spec


let balance =
  let open Command.Param in
  let name = "--balance" in
  let doc =
    "INT the balance the Michelson interpreter will use for the contract balance."
  in
  let spec = optional_with_default "0" string in
  flag ~doc name spec


let sender =
  let open Command.Param in
  let name = "--sender" in
  let doc = "ADDRESS the sender the Michelson interpreter transaction will use." in
  let spec = optional string in
  flag ~doc name spec


let source =
  let open Command.Param in
  let name = "--source" in
  let doc = "ADDRESS the source the Michelson interpreter transaction will use." in
  let spec = optional string in
  flag ~doc name spec


let allow_json_download =
  let open Command.Param in
  let name = "--allow-json-download" in
  let doc = "Allow LIGO to download JSON files for metadata check." in
  flag ~doc name no_arg


let disallow_json_download =
  let open Command.Param in
  let name = "--disallow-json-download" in
  let doc =
    "Disallow LIGO to download JSON files for metadata check (and do not show message)."
  in
  flag ~doc name no_arg


let disable_michelson_typechecking =
  let open Command.Param in
  let name = "--disable-michelson-typechecking" in
  let doc = "Disable Michelson typecking, this might produce ill-typed Michelson code." in
  flag ~doc name no_arg


let only_ep =
  let open Command.Param in
  let name = "--only-ep" in
  let doc = "Only display declarations that have the type of an entrypoint" in
  flag ~doc name no_arg


let skip_generated =
  let open Command.Param in
  let name = "--skip-generated" in
  let doc = "Skip generated declarations" in
  flag ~doc name no_arg


let experimental_disable_optimizations_for_debugging =
  let open Command.Param in
  let name = "--experimental-disable-optimizations-for-debugging" in
  let doc =
    "Experimental: Disable certain optimizations in order to simplify the relationship \
     between the source LIGO and the target Michelson. Intended for use with stepwise \
     Michelson debuggers."
  in
  flag ~doc name no_arg


let enable_michelson_typed_opt =
  let open Command.Param in
  let name = "--enable-michelson-typed-opt" in
  let doc = "Enable Michelson optimizations that work using typecking." in
  flag ~doc name no_arg


let without_run =
  let open Command.Param in
  let name = "--without-run" in
  let doc = "disable running of compiled expression." in
  flag ~doc name no_arg


let no_stdlib =
  let open Command.Param in
  let name = "--no-stdlib" in
  let doc = "disable stdlib inclusion." in
  flag ~doc name no_arg


let with_types =
  let open Command.Param in
  let name = "--with-types" in
  let doc = "Tries to infer types for all named expressions" in
  flag ~doc name no_arg


let defs_only =
  let open Command.Param in
  let name = "--defs-only" in
  let doc = "Gets only list of definitions (without scopes)." in
  flag ~doc name no_arg


let disable_lsp_request_logging =
  let open Command.Param in
  let name = "--disable-lsp-requests-logging" in
  let doc = "Disables request logging for LSP server." in
  flag ~doc name no_arg


let now =
  let open Command.Param in
  let name = "--now" in
  let doc =
    "TIMESTAMP the NOW value the Michelson interpreter will use (e.g. \
     '2000-01-01T10:10:10Z')"
  in
  let spec = optional string in
  flag ~doc name spec


let no_colour =
  let open Command.Param in
  (* Using the american standard for the CLI *)
  let name = "--no-color" in
  let doc = "disable coloring in CLI output" in
  flag ~doc name no_arg


let no_metadata_check =
  let open Command.Param in
  let name = "--no-metadata-check" in
  let doc = "disable TZIP-16 metadata compliance check" in
  flag ~doc name no_arg


let show_loc =
  let open Command.Param in
  (* Using the american standard for the CLI *)
  let name = "--show-loc" in
  let doc = "show location in s-expressions" in
  flag ~doc name no_arg


let hide_sort : _ Command.Param.t =
  let open Command.Param in
  let all_sorts =
    [ "ty_expr"
    ; "pattern"
    ; "instruction"
    ; "statement"
    ; "block"
    ; "declaration"
    ; "mod_expr"
    ; "expr"
    ; "program"
    ; "program_entry"
    ]
  in
  let doc =
    Format.asprintf
      "restrict sorts shown in s-exp. available sorts: %a"
      (Simple_utils.PP_helpers.list String.pp)
      all_sorts
  in
  let spec =
    optional_with_default []
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string
  in
  flag ~doc ~aliases:[ "hide" ] "--hide-sort" spec


let function_body =
  let open Command.Param in
  let name = "--function-body" in
  let doc = "compile expression as a function body" in
  flag ~doc name no_arg


let display_format =
  let open Command.Param in
  let open Simple_utils.Display in
  let name = "--display-format" in
  let doc =
    "FORMAT the format that will be used by the CLI. Available formats are 'dev', \
     'json', and 'human-readable' (default). When human-readable lacks details (we are \
     still tweaking it), please contact us and use another format in the meanwhile."
  in
  flag ~doc ~aliases:[ "--format" ] name
  @@ optional_with_default human_readable
  @@ Command.Arg_type.create
  @@ function
  | "human-readable" -> human_readable
  | "dev" -> dev
  | "json" -> json
  | _ -> failwith "todo"


let output_file =
  let open Command.Param in
  let doc =
    "FILENAME if used, prints the output into the specified file instead of stdout"
  in
  let spec = optional string in
  flag ~doc ~aliases:[ "o" ] "--output-file" spec


let michelson_code_format =
  let open Command.Param in
  let docv = "--michelson-format" in
  let doc =
    "CODE_FORMAT format that will be used by compile-contract for the resulting \
     Michelson. Available formats are 'text' (default), 'json', 'msgpack' and 'hex'."
  in
  flag ~doc docv
  @@ optional_with_default `Text
  @@ Command.Arg_type.create
  @@ function
  | "text" -> `Text
  | "json" -> `Json
  | "hex" -> `Hex
  | "msgpack" -> `Msgpack
  | _ -> failwith "todo"


let michelson_comments =
  let open Command.Param in
  let doc =
    "COMMENT_TYPE Selects kinds of comments to be added to the Michelson output. \
     Currently 'location' and 'env' are supported. 'location' propagates original source \
     locations. 'env' inserts additional empty Seq nodes with comments relating the \
     Michelson stack to the source LIGO environment."
  in
  let name = "--michelson-comments" in
  flag ~doc name
  @@ listed
  @@ Command.Arg_type.create (function
         | "location" -> `Location
         | "env" -> `Env
         | s -> failwithf "unexpected value for --%s: %s" name s ())


let optimize =
  let open Command.Param in
  let name = "--optimize" in
  let doc = "ENTRY_POINT Apply Mini-C optimizations as if compiling ENTRY_POINT" in
  flag ~doc name @@ optional string


let test_mode =
  let open Command.Param in
  let name = "--test" in
  let doc = "force testing mode." in
  flag ~doc name no_arg


let warn =
  let open Command.Param in
  let name = "--no-warn" in
  let doc = "disable warning messages" in
  map ~f:not @@ flag ~doc name no_arg


let warn_unused_rec =
  let open Command.Param in
  let name = "--warn-unused-rec" in
  let doc = "warn about unused recursion in a recursive function" in
  flag ~doc name no_arg


let warn_infinite_loop =
  let open Command.Param in
  let name = "--warn-infinite-loop" in
  let doc = "warn about infinite loop" in
  flag ~doc name no_arg


let werror =
  let open Command.Param in
  let name = "--werror" in
  let doc = "treat warnings as errors" in
  flag ~doc name no_arg


let self_pass =
  let open Command.Param in
  let name = "--self-pass" in
  let doc = "apply the self pass" in
  flag ~doc name no_arg


let dry_run_flag =
  let open Command.Param in
  let name = "--dry-run" in
  let doc = "don't publish changes to LIGO registry." in
  flag ~doc name no_arg


let project_root =
  let open Command.Param in
  let name = "--project-root" in
  let doc = "PATH The path to root of the project." in
  let spec = optional string in
  let spec =
    map_flag spec ~f:(function
        | None -> Cli_helpers.find_project_root ()
        | Some x -> Some x)
  in
  flag ~doc name spec


let transpiled =
  let open Command.Param in
  let name = "--transpiled" in
  let doc = "Disable checks that are unapplicable to transpiled contracts." in
  flag ~doc name no_arg


let cache_path =
  let open Command.Param in
  let name = "--cache-path" in
  let doc = "PATH The path where dependencies are installed." in
  let spec = optional_with_default Constants.ligo_install_path string in
  flag ~doc name spec


let command_arg_type_uri = Command.Arg_type.create Uri.of_string

let ligo_registry =
  let open Command.Param in
  let name = "--registry" in
  let doc = "URL The url to a LIGO registry." in
  let spec =
    optional_with_default (Uri.of_string Constants.ligo_registry) command_arg_type_uri
  in
  flag ~doc name spec


let ligorc_path =
  let open Command.Param in
  let name = "--ligorc-path" in
  let doc = "PATH path to .ligorc file." in
  let spec = optional_with_default (Constants.ligo_rc_path ()) string in
  flag ~doc name spec


let esy_legacy =
  let open Command.Param in
  let name = "--legacy-package-management" in
  let doc = "enable installing packages with legacy package manager esy" in
  flag ~doc name no_arg


module Api = Ligo_api

let ( <*> ) = Command.Param.( <*> )
let ( <$> ) f a = Command.Param.return f <*> a

(* Command run function of type () -> () and catches exception inside.
I use a mutable variable to propagate back the effect of the result of f *)
let return = ref Done
let reset_return () = return := Done

let compile_file =
  let f
      source_file
      entry_point
      module_
      views
      syntax
      protocol_version
      display_format
      disable_michelson_typechecking
      experimental_disable_optimizations_for_debugging
      enable_typed_opt
      no_stdlib
      michelson_format
      output_file
      show_warnings
      warning_as_error
      no_colour
      no_metadata_check
      skip_analytics
      michelson_comments
      constants
      file_constants
      project_root
      transpiled
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~module_
        ~syntax
        ~protocol_version
        ~disable_michelson_typechecking
        ~experimental_disable_optimizations_for_debugging
        ~enable_typed_opt
        ~no_stdlib
        ~warning_as_error
        ~no_colour
        ~no_metadata_check
        ~constants
        ~file_constants
        ~project_root
        ~transpiled
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"compile_contract"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
      ?output_file
    @@ Api.Compile.contract
         raw_options
         entry_point
         (Api.Compile.File source_file)
         michelson_format
         michelson_comments
         views
  in
  let summary = "compile a contract." in
  let readme () =
    "This sub-command compiles a contract to Michelson code. It expects a source file \
     and an entrypoint function that has the type of a contract: \"parameter * storage \
     -> operations list * storage\"."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> entry_point
    <*> module_
    <*> on_chain_views
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> disable_michelson_typechecking
    <*> experimental_disable_optimizations_for_debugging
    <*> enable_michelson_typed_opt
    <*> no_stdlib
    <*> michelson_code_format
    <*> output_file
    <*> warn
    <*> werror
    <*> no_colour
    <*> no_metadata_check
    <*> skip_analytics
    <*> michelson_comments
    <*> constants
    <*> file_constants
    <*> project_root
    <*> transpiled
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let compile_parameter =
  let f
      source_file
      parameter_entrypoint_opt
      module_
      expression
      syntax
      protocol_version
      amount
      balance
      sender
      source
      now
      display_format
      no_colour
      skip_analytics
      michelson_format
      output_file
      show_warnings
      warning_as_error
      constants
      file_constants
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~module_
        ~protocol_version
        ~warning_as_error
        ~constants
        ~file_constants
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"compile_parameter"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
      ?output_file
    @@ Api.Compile.parameter
         raw_options
         parameter_entrypoint_opt
         source_file
         expression
         amount
         balance
         sender
         source
         now
         michelson_format
  in
  let summary = "compile parameters to a Michelson expression." in
  let readme () =
    "This sub-command compiles a parameter for a given contract to a Michelson \
     expression. The resulting Michelson expression can be passed as an argument in a \
     transaction which calls a contract."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> parameter_entrypoint
    <*> module_
    <*> expression "parameter"
    <*> syntax
    <*> protocol_version
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> michelson_code_format
    <*> output_file
    <*> warn
    <*> werror
    <*> constants
    <*> file_constants
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let compile_expression =
  let f
      syntax
      expression
      protocol_version
      init_file
      display_format
      no_colour
      skip_analytics
      without_run
      no_stdlib
      michelson_format
      show_warnings
      warning_as_error
      constants
      file_constants
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      function_body
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~without_run
        ~no_stdlib
        ~warning_as_error
        ~constants
        ~file_constants
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ~function_body
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"compile_expression"
        ~raw_options
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Compile.expression raw_options expression init_file michelson_format
  in
  let summary = "compile to a Michelson value." in
  let readme () =
    "This sub-command compiles a LIGO expression to a Michelson value. It works by \
     compiling the LIGO expression to a Michelson expression and then interpreting it \
     using Michelson's interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> req_syntax
    <*> expression ""
    <*> protocol_version
    <*> init_file
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> without_run
    <*> no_stdlib
    <*> michelson_code_format
    <*> warn
    <*> werror
    <*> constants
    <*> file_constants
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries
    <*> function_body)


let compile_storage =
  let f
      source_file
      expression
      entry_point
      module_
      syntax
      protocol_version
      amount
      balance
      sender
      source
      now
      display_format
      no_colour
      no_metadata_check
      allow_json_download
      disallow_json_download
      skip_analytics
      michelson_format
      output_file
      show_warnings
      warning_as_error
      constants
      file_constants
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let json_download =
      if disallow_json_download
      then Some false
      else if allow_json_download
      then Some true
      else None
    in
    let raw_options =
      Raw_options.make
        ~module_
        ~syntax
        ~protocol_version
        ~warning_as_error
        ~constants
        ~file_constants
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ~no_metadata_check
        ~json_download
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"compile_storage"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
      ?output_file
    @@ Api.Compile.storage
         raw_options
         entry_point
         (From_file source_file)
         expression
         amount
         balance
         sender
         source
         now
         michelson_format
  in
  let summary = "compile an initial storage in LIGO syntax to a Michelson expression." in
  let readme () =
    "This sub-command compiles an initial storage for a given contract to a Michelson \
     expression. The resulting Michelson expression can be passed as an argument in a \
     transaction which originates a contract."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> expression "STORAGE"
    <*> entry_point
    <*> module_
    <*> syntax
    <*> protocol_version
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> display_format
    <*> no_colour
    <*> no_metadata_check
    <*> allow_json_download
    <*> disallow_json_download
    <*> skip_analytics
    <*> michelson_code_format
    <*> output_file
    <*> warn
    <*> werror
    <*> constants
    <*> file_constants
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let compile_constant =
  let f
      syntax
      expression
      protocol_version
      init_file
      display_format
      no_colour
      skip_analytics
      without_run
      show_warnings
      warning_as_error
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~without_run
        ~warning_as_error
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"compile_constant"
        ~raw_options
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Compile.constant raw_options expression init_file
  in
  let summary = "compile constant to a Michelson value and its hash." in
  let readme () =
    "This sub-command compiles a LIGO expression to a Michelson value and its hash as a \
     global constant. It works by compiling the LIGO expression to a Michelson \
     expression and then interpreting it using Michelson's interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> req_syntax
    <*> expression ""
    <*> protocol_version
    <*> init_file
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> without_run
    <*> warn
    <*> werror
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let compile_group =
  Command.group ~summary:"compile a ligo program to michelson"
  @@ [ "contract", compile_file
     ; "expression", compile_expression
     ; "parameter", compile_parameter
     ; "storage", compile_storage
     ; "constant", compile_constant
     ]


(** Transpile commands *)
let transpile_contract =
  let f
      source_file
      to_syntax
      from_syntax
      display_format
      no_colour
      skip_analytics
      output_file
      ()
    =
    let cli_analytic = Analytics.generate_cli_metric ~command:"transpile_contract" in
    let transpile_analytic =
      Analytics.
        { group =
            Counter_cli_transpile
              { command = "transpile_contract"
              ; old_syntax = Analytics.determine_syntax_label from_syntax source_file
              ; new_syntax = to_syntax
              }
        ; metric_value = 1.0
        }
    in
    return_result
      ~skip_analytics
      ~cli_analytics:[ cli_analytic; transpile_analytic ]
      ~return
      ?output_file
      ~display_format
      ~no_colour
      ~warning_as_error:false
    @@ Api.Transpile.contract source_file to_syntax from_syntax output_file
  in
  let summary = "Transpile a contract to another syntax." in
  let readme () =
    "This sub-command transpiles a source file to another syntax.It parses the source \
     file and performs the transpiling at the syntactic level.It can be used for \
     transpiling PascaLIGO contracts to JsLIGO."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> to_syntax
    <*> from_syntax
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> output_file)


let transpile_group =
  Command.group ~summary:"Transpile ligo code from a syntax to another"
  @@ [ "contract", transpile_contract ]


(** Transpile with AST commands *)
let transpile_with_ast_contract =
  let f
      source_file
      new_syntax
      syntax
      display_format
      skip_analytics
      no_colour
      output_file
      libraries
      ()
    =
    let cli_analytic =
      Analytics.generate_cli_metric ~command:"transpile_with_ast_contract"
    in
    let transpile_analytic =
      Analytics.
        { group =
            Counter_cli_transpile
              { command = "transpile_with_ast_contract"
              ; old_syntax = Analytics.determine_syntax_label syntax source_file
              ; new_syntax
              }
        ; metric_value = 1.0
        }
    in
    return_result
      ~skip_analytics
      ~cli_analytics:[ cli_analytic; transpile_analytic ]
      ~return
      ?output_file
      ~display_format
      ~no_colour
      ~warning_as_error:false
    @@ Api.Transpile_with_ast.contract source_file new_syntax syntax libraries
  in
  let summary =
    "[BETA] transpile a contract to another syntax, compiling down to the AST and then \
     decompiling."
  in
  let readme () =
    "[BETA] This sub-command transpiles a source file to another syntax. It does not use \
     the build system, but the source file is preprocessed. Comments are currently not \
     transpiled. Please use at your own risk."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> req_syntax
    <*> syntax
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> output_file
    <*> libraries)


let transpile_with_ast_expression =
  let f syntax expression new_syntax display_format no_colour skip_analytics libraries () =
    let cli_analytic =
      Analytics.generate_cli_metric ~command:"transpile_with_ast_expression"
    in
    let transpile_analytic =
      Analytics.
        { group =
            Counter_cli_transpile
              { command = "transpile_with_ast_expression"
              ; old_syntax = syntax
              ; new_syntax
              }
        ; metric_value = 1.0
        }
    in
    return_result
      ~skip_analytics
      ~cli_analytics:[ cli_analytic; transpile_analytic ]
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:false
    @@ Api.Transpile_with_ast.expression expression new_syntax syntax libraries
  in
  let summary = "[BETA] transpile an expression to another syntax." in
  let readme () =
    "[BETA] This sub-command transpiles a LIGO expression to another syntax. Comments \
     are currently not transpiled. Please use at your own risk."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> req_syntax
    <*> expression ""
    <*> req_syntax
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> libraries)


let transpile_with_ast_group =
  Command.group ~summary:"[BETA] transpile ligo code from a syntax to another"
  @@ [ "contract", transpile_with_ast_contract
     ; "expression", transpile_with_ast_expression
     ]


(** Run commands *)
let test =
  let f
      source_file
      syntax
      steps
      cli_expr_inj
      display_format
      no_colour
      skip_analytics
      show_warnings
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~steps
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~cli_expr_inj
        ~test:true
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"run_test"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.test raw_options (Build.Source_input.From_file source_file)
  in
  let summary = "test a contract with the LIGO test framework." in
  let readme () =
    "This sub-command tests a LIGO contract using a LIGO interpreter. Still under \
     development, there are features that are work in progress and are subject to \
     change. No real test procedure should rely on this sub-command alone."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> syntax
    <*> steps
    <*> cli_expr_inj
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> warn
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let test_expr =
  let f
      syntax
      expr
      source_file
      steps
      cli_expr_inj
      display_format
      no_colour
      skip_analytics
      show_warnings
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~steps
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~cli_expr_inj
        ~test:true
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"run_test-expr"
        ~raw_options
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.test_expression raw_options expr source_file
  in
  let summary = "test a expression with the LIGO test framework." in
  let readme () =
    "This sub-command tests a LIGO contract using a LIGO interpreter. Still under \
     development, there are features that are work in progress and are subject to \
     change. No real test procedure should rely on this sub-command alone."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> req_syntax
    <*> expression ""
    <*> init_file
    <*> steps
    <*> cli_expr_inj
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> warn
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let dry_run =
  let f
      source_file
      parameter
      storage
      entry_point
      module_
      amount
      balance
      sender
      source
      now
      syntax
      protocol_version
      display_format
      no_colour
      skip_analytics
      show_warnings
      warning_as_error
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~module_
        ~syntax
        ~protocol_version
        ~warning_as_error
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"run_dry-run"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.dry_run
         raw_options
         entry_point
         source_file
         parameter
         storage
         amount
         balance
         sender
         source
         now
  in
  let summary = "run a smart-contract with the given storage and input." in
  let readme () =
    "This sub-command runs a LIGO contract on a given storage and parameter. The context \
     is initialized from a source file where the contract is implemented. The \
     interpretation is done using Michelson's interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> expression "PARAMETER"
    <*> expression "STORAGE"
    <*> entry_point
    <*> module_
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> warn
    <*> werror
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let evaluate_call =
  let f
      source_file
      function_name
      parameter
      amount
      balance
      sender
      source
      now
      syntax
      protocol_version
      display_format
      no_colour
      skip_analytics
      show_warnings
      warning_as_error
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~warning_as_error
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"run_evaluate-call"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.evaluate_call
         raw_options
         source_file
         function_name
         parameter
         amount
         balance
         sender
         source
         now
  in
  let summary = "run a function with the given parameter." in
  let readme () =
    "This sub-command runs a LIGO function on a given argument. The context is \
     initialized from a source file where the function is implemented. The \
     interpretation is done using Michelson's interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> function_name
    <*> expression "PARAMETER"
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> warn
    <*> werror
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let evaluate_expr =
  let f
      source_file
      exp
      amount
      balance
      sender
      source
      now
      syntax
      protocol_version
      display_format
      no_colour
      skip_analytics
      show_warnings
      warning_as_error
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~warning_as_error
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"run_evaluate-expr"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.evaluate_expr raw_options source_file exp amount balance sender source now
  in
  let summary = "evaluate a given definition." in
  let readme () =
    "This sub-command evaluates a LIGO definition. The context is initialized from a \
     source file where the definition is written. The interpretation is done using a \
     Michelson interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> expression "EXPR"
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> warn
    <*> werror
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let interpret =
  let f
      expression
      init_file
      syntax
      protocol_version
      amount
      balance
      sender
      source
      now
      display_format
      no_colour
      skip_analytics
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~libraries
        ()
    in
    let cli_analytics =
      match init_file with
      | Some file ->
        Analytics.generate_cli_metrics_with_syntax_and_protocol
          ~command:"run_interpret"
          ~raw_options
          ~source_file:file
          ()
      | None ->
        Analytics.generate_cli_metrics_with_syntax_and_protocol
          ~command:"run_interpret"
          ~raw_options
          ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Run.interpret raw_options expression init_file amount balance sender source now
  in
  let summary =
    "interpret the expression in the context initialized by the provided source file."
  in
  let readme () =
    "This sub-command interprets a LIGO expression. The context can be initialized by \
     providing a source file. The interpretation is done using Michelson's interpreter."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> expression "EXPRESSION"
    <*> init_file
    <*> syntax
    <*> protocol_version
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let run_group =
  Command.group
    ~summary:"compile and interpret ligo code"
    [ "test", test
    ; "test-expr", test_expr
    ; "dry-run", dry_run
    ; "evaluate-call", evaluate_call
    ; "evaluate-expr", evaluate_expr
    ; "interpret", interpret
    ]


(** Info commands *)
let list_declarations =
  let f
      source_file
      only_ep
      skip_generated
      syntax
      display_format
      no_colour
      skip_analytics
      project_root
      libraries
      ()
    =
    let raw_options =
      Raw_options.make ~only_ep ~skip_generated ~syntax ~project_root ~libraries ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"info_list-declarations"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Info.list_declarations raw_options source_file
  in
  let summary = "list all the top-level declarations." in
  let readme () =
    "This sub-command prints a list of all top-level declarations (not including types \
     and modules)."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> only_ep
    <*> skip_generated
    <*> syntax
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> project_root
    <*> libraries)


let measure_contract =
  let f
      source_file
      entry_point
      syntax
      protocol_version
      display_format
      no_colour
      skip_analytics
      enable_typed_opt
      show_warnings
      warning_as_error
      project_root
      warn_unused_rec
      warn_infinite_loop
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~warning_as_error
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~enable_typed_opt
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"info_measure-contract"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics
      ~cli_analytics
      ~return
      ~show_warnings
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Info.measure_contract raw_options entry_point source_file
  in
  let summary = "measure a contract's compiled size in bytes." in
  let readme () =
    "This sub-command compiles a source file and measures the contract's compiled size \
     in bytes."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> entry_point
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> enable_michelson_typed_opt
    <*> warn
    <*> werror
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> libraries)


let get_scope =
  let f
      source_file
      protocol_version
      libraries
      display_format
      no_colour
      with_types
      defs_only
      project_root
      no_stdlib
      ()
    =
    let raw_options =
      Raw_options.make
        ~protocol_version
        ~libraries
        ~with_types
        ~defs_only
        ~project_root
        ~no_stdlib
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"info_get-scope"
        ~raw_options
        ~source_file
        ()
    in
    return_with_custom_formatter ~skip_analytics:false ~cli_analytics ~return
    @@ fun () ->
    Lsp_helpers.Ligo_interface.Get_scope.get_scope_cli_result
      raw_options
      ~source_file
      ~display_format
      ~no_colour
      ~defs_only
  in
  let summary = "return the JSON encoded environment for a given file." in
  let readme () =
    "This sub-command returns the environment for a given file in JSON format. It does \
     not use the build system."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> protocol_version
    <*> libraries
    <*> display_format
    <*> no_colour
    <*> with_types
    <*> defs_only
    <*> project_root
    <*> no_stdlib)


let resolve_config =
  let f source_file display_format () =
    let raw_options = Raw_options.make () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"info_resolve-config"
        ~raw_options
        ~source_file
        ()
    in
    return_result_lwt
      ~skip_analytics:true
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour:true
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Info.resolve_config raw_options source_file
  in
  let summary = "Resolves a config for the LIGO debugger" in
  let readme () =
    "This sub-command resolves a configuration written in LIGO which can be used for the \
     LIGO debugger.\n\n\
    \    For more information, read the debugger's read me."
  in
  Command.basic ~summary ~readme (f <$> source_file <*> display_format)


let dump_cst =
  let f
      (source_file, source_files)
      protocol_version
      libraries
      display_format
      with_types
      skip_analytics
      no_colour
      ()
    =
    let raw_options = Raw_options.make ~protocol_version ~libraries ~with_types () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"info_dump-cst"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~return
      ~display_format
      ~skip_analytics
      ~no_colour
      ~cli_analytics
      ~warning_as_error:raw_options.warning_as_error
      ~minify_json:true
    @@ Api.Info.dump_cst raw_options (source_file :: source_files)
  in
  let summary = "dump CST for a contract." in
  let readme () =
    "This subcommand returns a concrete syntax tree for a LIGO contract. If format is \
     not specified, then CST would be returned in the MessagePack format."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_files
    <*> protocol_version
    <*> libraries
    <*> display_format
    <*> with_types
    <*> skip_analytics
    <*> no_colour)


let info_group =
  let summary = "tools to get information from contracts" in
  Command.group
    ~summary
    [ "list-declarations", list_declarations
    ; "measure-contract", measure_contract
    ; "get-scope", get_scope
    ; "resolve-config", resolve_config
    ; "dump-cst", dump_cst
    ]


(** Print commands *)
let preprocessed =
  let f
      source_file
      syntax
      libraries
      display_format
      project_root
      no_colour
      skip_analytics
      ()
    =
    let raw_options = Raw_options.make ~syntax ~libraries ~project_root ~no_colour () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_preprocessed"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.preprocess raw_options source_file
  in
  let summary =
    "preprocess the source file.\n\
     Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command runs the pre-processor on a LIGO source file and outputs the \
     result. The directive `#include` directly inlines the included file and therefore \
     its content appears in the output. In contrast, the directive `#import` includes \
     the file as a module and therefore the content of the imported file is not printed \
     by this sub-command."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> libraries
     <*> display_format
     <*> project_root
     <*> no_colour
     <*> skip_analytics)


let pretty_print =
  let f
      source_file
      syntax
      display_format
      warning_as_error
      no_colour
      skip_analytics
      project_root
      libraries
      ()
    =
    let raw_options =
      Raw_options.make ~syntax ~warning_as_error ~no_colour ~project_root ~libraries ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_pretty"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.pretty_print raw_options source_file
  in
  let summary = "pretty-print the source file." in
  let readme () =
    "This sub-command pretty-prints a source file in LIGO. The width of the \
     pretty-printed text is adjusted to the number of columns in the terminal (or 60 if \
     it cannot be determined)."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> display_format
     <*> werror
     <*> no_colour
     <*> skip_analytics
     <*> project_root
     <*> libraries)


let print_graph =
  let f
      source_file
      syntax
      display_format
      project_root
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options = Raw_options.make ~syntax ~project_root ~no_colour ~libraries () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_dependency-graph"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.dependency_graph raw_options source_file
  in
  let summary =
    "print the dependency graph.\n\
     Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the dependency graph created by the module system. It \
     explores all imported source files (recursively) following a DFS strategy."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> display_format
     <*> project_root
     <*> no_colour
     <*> skip_analytics
     <*> libraries)


let print_cst =
  let f
      source_file
      syntax
      display_format
      no_colour
      skip_analytics
      project_root
      libraries
      ()
    =
    let raw_options = Raw_options.make ~syntax ~no_colour ~project_root ~libraries () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_cst"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.cst raw_options source_file
  in
  let summary =
    "print the CST.\nWarning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the CST stage, obtained after \
     preprocessing and parsing."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> display_format
     <*> no_colour
     <*> skip_analytics
     <*> project_root
     <*> libraries)


let print_ast_unified =
  let f
      source_file
      syntax
      nanopass
      display_format
      no_colour
      show_loc
      hide_sort
      skip_analytics
      project_root
      libraries
      ()
    =
    let raw_options = Raw_options.make ~syntax ~no_colour ~project_root ~libraries () in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-imperative"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.ast_unified raw_options show_loc hide_sort nanopass source_file
  in
  let summary =
    "print the unified ligo AST. Execute nanopasses if option used\n\
    \ Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the AST unified stage (with nanopasses)."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> nanopass
     <*> display_format
     <*> no_colour
     <*> show_loc
     <*> hide_sort
     <*> skip_analytics
     <*> project_root
     <*> libraries)


let print_ast_core =
  let f
      source_file
      syntax
      display_format
      self_pass
      project_root
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options =
      Raw_options.make ~syntax ~self_pass ~project_root ~no_colour ~libraries ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-core"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.ast_core raw_options source_file
  in
  let summary =
    "print the core ligo AST.\n\
    \ Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () = "This sub-command prints the source file in the AST core stage." in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> display_format
     <*> self_pass
     <*> project_root
     <*> no_colour
     <*> skip_analytics
     <*> libraries)


let print_ast_typed =
  let f
      source_file
      syntax
      protocol_version
      display_format
      self_pass
      project_root
      warn_unused_rec
      warn_infinite_loop
      test
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~self_pass
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~test
        ~no_colour
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-typed"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.ast_typed raw_options source_file
  in
  let summary =
    "print the typed AST.\n\
    \ Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the AST typed stage. Internally, it uses \
     the build system to type the contract, but the contract is not combined with \
     imported modules."
  in
  Command.basic ~summary ~readme
  @@ (f
     <$> source_file
     <*> syntax
     <*> protocol_version
     <*> display_format
     <*> self_pass
     <*> project_root
     <*> warn_unused_rec
     <*> warn_infinite_loop
     <*> test_mode
     <*> no_colour
     <*> skip_analytics
     <*> libraries)


let print_ast_aggregated =
  let f
      source_file
      syntax
      protocol_version
      display_format
      self_pass
      project_root
      warn_unused_rec
      warn_infinite_loop
      test
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~self_pass
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~test
        ~no_colour
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-options"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.ast_aggregated raw_options source_file
  in
  let summary =
    "print the contract after aggregation.\n\
    \ Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the AST aggregated stage."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> self_pass
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> test_mode
    <*> no_colour
    <*> skip_analytics
    <*> libraries)


let print_module_signature =
  let f
      source_file
      syntax
      module_
      protocol_version
      display_format
      self_pass
      project_root
      warn_unused_rec
      warn_infinite_loop
      test
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~self_pass
        ~module_
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~test
        ~no_colour
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-options"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.signature raw_options source_file
  in
  let summary = "print the file signature in the desired syntax" in
  let readme () = "This sub-command prints the source file signature." in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> syntax
    <*> module_
    <*> protocol_version
    <*> display_format
    <*> self_pass
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> test_mode
    <*> no_colour
    <*> skip_analytics
    <*> libraries)


let print_ast_expanded =
  let f
      source_file
      syntax
      protocol_version
      display_format
      no_colour
      skip_analytics
      self_pass
      project_root
      warn_unused_rec
      warn_infinite_loop
      test
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~self_pass
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~test
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_ast-expanded"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.ast_expanded raw_options source_file
  in
  let summary =
    "print the contract after aggregation.\n\
    \ Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the AST aggregated stage."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> self_pass
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> test_mode
    <*> libraries)


let print_mini_c =
  let f
      source_file
      syntax
      protocol_version
      display_format
      optimize
      project_root
      warn_unused_rec
      warn_infinite_loop
      no_colour
      skip_analytics
      libraries
      ()
    =
    let raw_options =
      Raw_options.make
        ~syntax
        ~protocol_version
        ~project_root
        ~warn_unused_rec
        ~warn_infinite_loop
        ~no_colour
        ~libraries
        ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"print_mini-c"
        ~raw_options
        ~source_file
        ()
    in
    return_result
      ~skip_analytics
      ~cli_analytics
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:raw_options.warning_as_error
    @@ Api.Print.mini_c raw_options source_file optimize
  in
  let summary =
    "print Mini-C. Warning: Intended for development of LIGO and can break at any time."
  in
  let readme () =
    "This sub-command prints the source file in the Mini-C stage. Internally, it uses \
     the build system to type and compile the contract. Compilation is applied after \
     combination in the AST typed stage."
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> source_file
    <*> syntax
    <*> protocol_version
    <*> display_format
    <*> optimize
    <*> project_root
    <*> warn_unused_rec
    <*> warn_infinite_loop
    <*> no_colour
    <*> skip_analytics
    <*> libraries)


let print_group =
  let summary =
    "print intermediary program representation.\n\
     Warning: Intended for development of LIGO and can break at any time"
  in
  Command.group ~summary ~preserve_subcommand_order:()
  @@ [ "preprocessed", preprocessed
     ; "pretty", pretty_print
     ; "signature", print_module_signature
     ; "dependency-graph", print_graph
     ; "cst", print_cst
     ; "ast-core", print_ast_core
     ; "ast-unified", print_ast_unified
     ; "ast-typed", print_ast_typed
     ; "ast-aggregated", print_ast_aggregated
     ; "ast-expanded", print_ast_expanded
     ; "mini-c", print_mini_c
     ]


(** init *)
let init_library =
  let f
      project_name
      template
      (template_list : bool)
      display_format
      no_colour
      skip_analytics
      registry
      ()
    =
    let cli_analytic = Analytics.generate_cli_metric ~command:"init_library" in
    let init_analytic =
      Analytics.
        { group = Counter_cli_init { command = "init_library"; template }
        ; metric_value = 1.0
        }
    in
    if template_list
    then
      return_with_custom_formatter
        ~skip_analytics:true
        ~cli_analytics:[ cli_analytic ]
        ~return
      @@ Ligo_init.list ~kind:`LIBRARY ~display_format ~no_colour
    else
      return_with_custom_formatter
        ~skip_analytics
        ~cli_analytics:[ cli_analytic; init_analytic ]
        ~return
      @@ Ligo_init.new_project
           ~version:Version.version
           ~kind:`LIBRARY
           ~project_name_opt:project_name
           ~template
           ~display_format
           ~no_colour
           ~registry
  in
  let summary = "Generate new folder which contains wished library template" in
  let readme () =
    "Generate new folder from library template. Internet connexion needed"
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> project_name
    <*> template
    <*> template_list
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> ligo_registry)


let init_contract =
  let f
      project_name
      template
      (template_list : bool)
      display_format
      no_colour
      skip_analytics
      registry
      ()
    =
    let cli_analytic = Analytics.generate_cli_metric ~command:"init_contract" in
    let init_analytic =
      Analytics.
        { group = Counter_cli_init { command = "init_contract"; template }
        ; metric_value = 1.0
        }
    in
    if template_list
    then
      return_with_custom_formatter
        ~skip_analytics:false
        ~cli_analytics:[ cli_analytic ]
        ~return
      @@ Ligo_init.list ~kind:`CONTRACT ~display_format ~no_colour
    else
      return_with_custom_formatter
        ~skip_analytics
        ~cli_analytics:[ cli_analytic; init_analytic ]
        ~return
      @@ Ligo_init.new_project
           ~version:Version.version
           ~kind:`CONTRACT
           ~project_name_opt:project_name
           ~template
           ~display_format
           ~no_colour
           ~registry
  in
  let summary = "Generate new folder which contains wished contract template" in
  let readme () =
    "Generate new folder from contract template. Internet connexion needed"
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> project_name
    <*> template
    <*> template_list
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> ligo_registry)


let init_group =
  Command.group
    ~summary:"Initialize a new ligo project from template. Contract or library."
    [ "library", init_library; "contract", init_contract ]


(** other *)
let changelog =
  let cli_analytic = Analytics.generate_cli_metric ~command:"changelog" in
  let f display_format no_colour skip_analytics () =
    return_result
      ~skip_analytics
      ~cli_analytics:[ cli_analytic ]
      ~return
      ~display_format
      ~no_colour
      ~warning_as_error:false
    @@ Api.dump_changelog ()
  in
  let summary = "print the ligo changelog" in
  let readme () = "Dump the LIGO changelog to stdout." in
  Command.basic ~summary ~readme (f <$> display_format <*> no_colour <*> skip_analytics)


let repl =
  let f
      syntax
      protocol_version
      amount
      balance
      sender
      source
      now
      display_format
      no_colour
      skip_analytics
      init_file
      project_root
      libraries
      ()
    =
    let raw_options =
      Raw_options.make ~syntax ~protocol_version ~project_root ~libraries ()
    in
    let cli_analytics =
      Analytics.generate_cli_metrics_with_syntax_and_protocol
        ~command:"repl"
        ~raw_options
        ()
    in
    return_with_custom_formatter ~skip_analytics ~cli_analytics ~return
    @@ Repl.main
         raw_options
         display_format
         no_colour
         now
         amount
         balance
         sender
         source
         init_file
  in
  let summary = "interactive ligo interpreter" in
  let readme () = "REPL (Read-Eval-Print-Loop) for LIGO" in
  Command.basic
    ~summary
    ~readme
    (f
    <$> req_syntax
    <*> protocol_version
    <*> amount
    <*> balance
    <*> sender
    <*> source
    <*> now
    <*> display_format
    <*> no_colour
    <*> skip_analytics
    <*> init_file
    <*> project_root
    <*> libraries)


let install =
  let summary = "install LIGO dependencies declared in ligo.json" in
  let readme () =
    "This command invokes the package manager to install the external packages declared \
     in ligo.json"
  in
  let cli_analytic = Analytics.generate_cli_metric ~command:"install" in
  let f project_root package_name cache_path ligo_registry esy_legacy skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () ->
    Install.install ~project_root ~package_name ~cache_path ~ligo_registry ~esy_legacy
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> project_root
    <*> package_name
    <*> cache_path
    <*> ligo_registry
    <*> esy_legacy
    <*> skip_analytics)


let registry_forgot_password =
  let summary =
    "Initiate a password resetting the password used to authenticate with the registry"
  in
  let readme () =
    "Initiate a password resetting the password used to authenticate with the registry"
  in
  let cli_analytic = Analytics.generate_cli_metric ~command:"forgot_password" in
  let f username ligo_registry ligorc_path skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () -> Forgot_password.main ~username ~ligo_registry ~ligorc_path
  in
  Command.basic
    ~summary
    ~readme
    (f <$> username <*> ligo_registry <*> ligorc_path <*> skip_analytics)


let registry_publish =
  let summary = "[BETA] publish the LIGO package declared in ligo.json" in
  let readme () =
    "[BETA] Packs the pacakage directory contents into a tarball and uploads it to the \
     registry server"
  in
  let cli_analytic = Analytics.generate_cli_metric ~command:"publish" in
  let f ligo_registry ligorc_path project_root dry_run skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () -> Publish.publish ~ligo_registry ~ligorc_path ~project_root ~dry_run
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> ligo_registry
    <*> ligorc_path
    <*> project_root
    <*> dry_run_flag
    <*> skip_analytics)


let registry_unpublish =
  let summary = "[BETA] unpublish the LIGO package" in
  let readme () = "[BETA] Unpublishes a package from the registry" in
  let cli_analytic = Analytics.generate_cli_metric ~command:"unpublish" in
  let f package_name package_version ligo_registry ligorc_path skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () ->
    Unpublish.unpublish
      ~name:package_name
      ~version:package_version
      ~ligo_registry
      ~ligorc_path
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> named_arg_package_name
    <*> named_arg_package_version
    <*> ligo_registry
    <*> ligorc_path
    <*> skip_analytics)


let username_optional =
  let open Command.Param in
  let name = "--username" in
  let doc = "Username registered with the registry" in
  let spec = optional string in
  flag ~doc name spec


let email_optional =
  let open Command.Param in
  let name = "--email" in
  let doc = "Email of the user registered with the registry" in
  let spec = optional string in
  flag ~doc name spec


let prompt_sensitive_with_env_fallback ~fallback_env_var ~prompt_msg =
  match Lwt_main.run @@ Prompt.prompt_sensitive ~fallback_env_var ~msg:prompt_msg with
  | Ok v -> Ok v
  | Error e -> Error (Prompt.error_to_string e, "")


let prompt_and_env_fallback ~fallback_env_var ~prompt_msg = function
  | Some v -> Ok v
  | None ->
    (match Sys.getenv fallback_env_var with
    | Some v -> Ok v
    | None ->
      (match Lwt_main.run @@ Prompt.prompt ~msg:prompt_msg with
      | Ok username -> Ok username
      | Error prompt_error -> Error (Prompt.error_to_string prompt_error, "")))


let add_user =
  let summary = "[BETA] create a new user for the LIGO package registry" in
  let readme () =
    "[BETA] Prompt the user for details to create a new user on registry server"
  in
  let cli_analytic = Analytics.generate_cli_metric ~command:"add-user" in
  let f username email ligo_registry ligorc_path skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () ->
    let ( let* ) = Caml.Result.bind in
    let* username =
      prompt_and_env_fallback
        ~fallback_env_var:"LIGO_USERNAME"
        ~prompt_msg:"Username: "
        username
    in
    let* email =
      prompt_and_env_fallback ~fallback_env_var:"LIGO_EMAIL" ~prompt_msg:"Email: " email
    in
    let* password =
      prompt_sensitive_with_env_fallback
        ~fallback_env_var:"USER_PASSWORD"
        ~prompt_msg:"Password: "
    in
    User.create ~username ~password ~email ~ligo_registry ~ligorc_path
  in
  Command.basic
    ~summary
    ~readme
    (f
    <$> username_optional
    <*> email_optional
    <*> ligo_registry
    <*> ligorc_path
    <*> skip_analytics)


let login =
  let summary = "[BETA] login to the LIGO package registry" in
  let readme () =
    "[BETA] Prompt the user for credentials to creates a login session with the registry \
     server"
  in
  let cli_analytic = Analytics.generate_cli_metric ~command:"login" in
  let f username ligo_registry ligorc_path skip_analytics () =
    return_with_custom_formatter ~skip_analytics ~cli_analytics:[ cli_analytic ] ~return
    @@ fun () ->
    let ( let* ) = Caml.Result.bind in
    let* username =
      prompt_and_env_fallback
        ~fallback_env_var:"LIGO_USERNAME"
        ~prompt_msg:"Username: "
        username
    in
    let* password =
      prompt_sensitive_with_env_fallback
        ~fallback_env_var:"USER_PASSWORD"
        ~prompt_msg:"Password: "
    in
    User.login ~username ~password ~ligo_registry ~ligorc_path
  in
  Command.basic
    ~summary
    ~readme
    (f <$> username_optional <*> ligo_registry <*> ligorc_path <*> skip_analytics)


let registry_group =
  Command.group ~summary:"Commands to interact with Ligo Package Registry"
  @@ [ "login", login
     ; "add-user", add_user
     ; "publish", registry_publish
     ; "unpublish", registry_unpublish
     ; "forgot-password", registry_forgot_password
     ]


module Lsp_server = struct
  (* Main code
  This is the code that creates an instance of the lsp server class
  and runs it as a task. *)

  module Requests = Ligo_lsp.Server.Requests
  module Server = Ligo_lsp.Server

  let run ?(log_requests = true) () =
    let run_lsp () =
      let s = new Server.lsp_server in
      let server = Linol_lwt.Jsonrpc2.create_stdio (s :> Linol_lwt.Jsonrpc2.server) in
      let shutdown () = Caml.(s#get_status = `ReceivedExit) in
      let task = Linol_lwt.Jsonrpc2.run ~shutdown server in
      match Linol_lwt.run task with
      | () -> Ok ("", "")
      | exception e ->
        let e = Caml.Printexc.to_string e in
        Error ("", e)
    in
    let with_request_logging f () =
      let reporter ppf =
        let report _src level ~over k msgf =
          let k _ =
            over ();
            k ()
          in
          let with_stamp header _tags k ppf fmt =
            let time = Time_ns.(to_string_utc @@ now ()) in
            Format.kfprintf
              k
              ppf
              ("%s %a @[" ^^ fmt ^^ "@]@.")
              time
              Logs.pp_header
              (level, header)
          in
          msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
        in
        { Logs.report }
      in
      let log_file = Filename.(temp_dir_name ^/ "ligo_language_server.log") in
      Out_channel.with_file ~append:true log_file ~f:(fun outc ->
          Logs.set_reporter (reporter @@ Format.formatter_of_out_channel outc);
          (* Disable logs for anything that is not linol, as it causes crashes. *)
          List.iter (Logs.Src.list ()) ~f:(fun src ->
              match Logs.Src.name src with
              | "linol" -> Logs.Src.set_level src (Some Logs.Debug)
              | _ -> Logs.Src.set_level src None);
          Logs.set_level (Some Logs.Debug);
          let s = new Server.lsp_server in
          let server = Linol_lwt.Jsonrpc2.create_stdio (s :> Linol_lwt.Jsonrpc2.server) in
          let shutdown () = Caml.(s#get_status = `ReceivedExit) in
          let task = Linol_lwt.Jsonrpc2.run ~shutdown server in
          Format.eprintf "For LIGO language server logs, see %s\n%!" log_file;
          match Linol_lwt.run task with
          | () -> Ok ("", "")
          | exception e ->
            let e = Caml.Printexc.to_string e in
            Error ("", e))
    in
    if log_requests then with_request_logging run_lsp () else run_lsp ()
end

let lsp =
  let summary = "[BETA] launch a LIGO lsp server" in
  let readme () = "[BETA] Run the lsp server which is used by editor extensions" in
  let f disable_logging () =
    let log_requests = not disable_logging in
    return_with_custom_formatter ~skip_analytics:true ~cli_analytics:[] ~return
    @@ Lsp_server.run ~log_requests
  in
  Command.basic ~summary ~readme (f <$> disable_lsp_request_logging)


let analytics_accept =
  let summary = "Accept analytics term" in
  let readme () = "Accept analytics term and store result in term_acceptance file" in
  let f () =
    return_with_custom_formatter ~skip_analytics:true ~cli_analytics:[] ~return
    @@ fun () -> Analytics.update_term_acceptance "accepted"
  in
  Command.basic ~summary ~readme (Command.Param.return f)


let analytics_deny =
  let summary = "Refuse analytics term" in
  let readme () = "Refuse analytics term and store result in term_acceptance file" in
  let f () =
    return_with_custom_formatter ~skip_analytics:true ~cli_analytics:[] ~return
    @@ fun () -> Analytics.update_term_acceptance "denied"
  in
  Command.basic ~summary ~readme (Command.Param.return f)


let analytics =
  Command.group
    ~summary:"Manage analytics"
    [ "accept", analytics_accept; "deny", analytics_deny ]


let main =
  Command.group ~preserve_subcommand_order:() ~summary:"The LigoLANG compiler"
  @@ [ "compile", compile_group
     ; "transpile", transpile_group
     ; "transpile-with-ast", transpile_with_ast_group
     ; "run", run_group
     ; "info", info_group
     ; "repl", repl
     ; "init", init_group
     ; "changelog", changelog
     ; "print", print_group
     ; "install", install
     ; "lsp", lsp
     ; "analytics", analytics
     ; "registry", registry_group
     ]


let run ?argv () =
  Printexc.record_backtrace true;
  let build_info =
    Format.sprintf
      "Protocol built-in: %s"
      Environment.Protocols.(variant_to_string in_use)
  in
  Command_unx.run ~build_info ~version:Version.version ?argv main;
  (* Effect to error code *)
  match !return with
  | Done -> 0
  | Compileur_Error -> 1
  | Exception exn ->
    ignore is_dev;
    raise exn
