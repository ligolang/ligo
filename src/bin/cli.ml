open Cli_helpers

module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let is_dev = ref true

let entry_point =
  let open Command.Param in
  let name = "e" in
  let doc = "ENTRY-POINT the entry-point that will be compiled." in
  let spec = optional_with_default Default_options.entry_point string in
  flag ~doc ~aliases:["--entry-point"] name spec

let source_file =
    let name = "SOURCE_FILE" in
    let _doc = "the path to the smart contract file." in
    Command.Param.(anon (name %: Filename_unix.arg_type))

let package_name =
  let name = "PACKAGE_NAME" in
  let _doc = "package to install." in
  Command.Param.(anon (maybe (name %: string)))

let expression purpose =
  let name = purpose ^ "_EXPRESSION" in
  let _desc = "the expression that will be compiled." in
  Command.Param.(anon (name %: string))

let libraries : string list Command.Param.t =
  let open Command.Param in
  let name = "--library" in
  let doc  = "LIBS A comma-separated list of paths to directories where to search for files to be included by the preprocessor" in
  let spec = optional_with_default Default_options.libraries
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string in
  flag ~doc ~aliases:["l"] name spec


let template =
  let open Command.Param in
  let doc  = "TEMPLATE the template name which will be used to generate folder. You can obtain available list by running ligo init list. If not provided default is empty-project." in
  let spec = optional_with_default "empty" string in
  flag ~doc ~aliases:["t"] "--template" spec

let template_list =
  let open Command.Param in
  let name = "--template-list" in
  let doc  = "If present, change cmmand behavior and list available templates for this command." in
  flag ~doc name no_arg

let project_name =
  let name = "PROJECT_NAME" in
  let _desc = "The generated project name" in
  Command.Param.(anon (maybe  (name %: string)))

let syntax =
  let open Command.Param in
  let doc  = "SYNTAX the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
  let spec = optional_with_default Default_options.syntax string in
  flag ~doc ~aliases:["s"] "--syntax" spec

let on_chain_views : _ Command.Param.t =
  let open Command.Param in
  let doc  = "VIEWS A list of declaration name that will be compiled as on-chain views, separated by ','" in
  let spec = optional_with_default Default_options.views
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string in
  flag ~doc ~aliases:["v"] "--views" spec

let constants : _ Command.Param.t =
  let open Command.Param in
  let doc  = "CONSTANTS A list of global constants that will be assumed in the context, separated by ','" in
  let spec = optional_with_default Default_options.constants
    @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string in
  flag ~doc ~aliases:["c"] "--constants" spec

let file_constants : _ Command.Param.t =
  let open Command.Param in
  let doc  = "FILE_CONSTANTS A file with a JSON list of strings with Michelson code. Those Michelson values will be registered as global constants in the context." in
  let spec = optional string in
  flag ~doc "--file-constants" spec

let steps =
  let open Command.Param in
  let doc  = "INT a bound in the number of steps to be done by the interpreter." in
  let spec = optional_with_default Default_options.steps int in
  flag ~doc ~aliases:["n"] "--steps" spec

let protocol_version =
  let open Command.Param in
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let doc   = Format.asprintf "PROTOCOL choose protocol's types/values pre-loaded into the LIGO environment %s. \
                              By default, the current protocol (%s) will be used" plist (variant_to_string current) in
  let spec  = optional_with_default Default_options.protocol_version string in
  flag ~doc ~aliases:["--protocol"] "p" spec

let cli_expr_inj =
  let open Command.Param in
  let doc  = "EXPRESSION a expression passed to LIGO interpreter, accessible through variable 'cli_arg'" in
  let spec = optional string in
  flag ~doc "--arg" spec

let req_syntax =
  let open Command.Param in
  let name = "SYNTAX" in
  let _desc = "the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively)." in
  anon (name %: string)
let init_file =
  let open Command.Param in
  let doc  = "FILENAME the path to the smart contract file to be used for context initialization." in
  let spec = optional string in
  flag ~doc "--init-file" spec

let amount =
  let open Command.Param in
  let name = "--amount" in
  let doc  = "INT the tezos amount the Michelson interpreter will use for the transaction." in
  let spec = optional_with_default "0" string in
  flag ~doc name spec
let balance =
  let open Command.Param in
  let name = "--balance" in
  let doc  = "INT the balance the Michelson interpreter will use for the contract balance." in
  let spec = optional_with_default "0" string in
  flag ~doc name spec

let sender =
  let open Command.Param in
  let name = "--sender" in
  let doc  = "ADDRESS the sender the Michelson interpreter transaction will use." in
  let spec = optional string in
  flag ~doc name spec

let source =
  let open Command.Param in
  let name = "--source" in
  let doc  = "ADDRESS the source the Michelson interpreter transaction will use." in
  let spec = optional string in
  flag ~doc name spec

let disable_michelson_typechecking =
  let open Command.Param in
  let name = "--disable-michelson-typechecking" in
  let doc  = "Disable Michelson typecking, this might produce ill-typed Michelson code." in
  flag ~doc name no_arg

let only_ep =
  let open Command.Param in
  let name = "--only-ep" in
  let doc  = "Only display declarations that have the type of an entrypoint" in
  flag ~doc name no_arg

let experimental_disable_optimizations_for_debugging =
  let open Command.Param in
  let name = "--experimental-disable-optimizations-for-debugging" in
  let doc  = "Experimental: Disable certain optimizations in order to \
              simplify the relationship between the source LIGO and \
              the target Michelson. Intended for use with stepwise \
              Michelson debuggers." in
  flag ~doc name no_arg

let enable_michelson_typed_opt =
  let open Command.Param in
  let name = "--enable-michelson-typed-opt" in
  let doc  = "Enable Michelson optimizations that work using typecking." in
  flag ~doc name no_arg

let without_run =
  let open Command.Param in
  let name = "--without-run" in
  let doc  = "disable running of compiled expression." in
  flag ~doc name no_arg

let no_stdlib =
  let open Command.Param in
  let name = "--no-stdlib" in
  let doc  = "disable stdlib inclusion." in
  flag ~doc name no_arg

let with_types =
  let open Command.Param in
  let name = "--with-types" in
  let doc = "Tries to infer types for all named expressions" in
  flag ~doc name no_arg

let now =
  let open Command.Param in
  let name = "--now" in
  let doc  = "TIMESTAMP the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
  let spec = optional string in
  flag ~doc name spec

let no_colour =
  let open Command.Param in
  let name = "--no-colour" in
  let doc = "disable coloring in CLI output" in
  flag ~doc name no_arg

let display_format =
  let open Command.Param in
  let open Simple_utils.Display in
  let name = "--display-format" in
  let doc  = "FORMAT the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
  flag ~doc ~aliases:["--format"] name @@
  optional_with_default human_readable @@
  Command.Arg_type.create @@ function
    | "human-readable" -> human_readable
    | "dev"            -> dev
    | "json"           -> json
    | _ -> failwith "todo"

let output_file =
  let open Command.Param in
  let doc  = "FILENAME if used, prints the output into the specified file instead of stdout" in
  let spec = optional string in
  flag ~doc ~aliases:["o"] "--output-file" spec

let michelson_code_format =
  let open Command.Param in
  let docv = "--michelson-format" in
  let doc = "CODE_FORMAT format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
  flag ~doc docv @@
  optional_with_default `Text @@
  Command.Arg_type.create @@ function
    | "text" -> `Text
    | "json" -> `Json
    | "hex"  -> `Hex
    | _ -> failwith "todo"

let michelson_comments =
  let open Command.Param in
  let doc = "COMMENT_TYPE Selects kinds of comments to be added to the Michelson output. \
Currently 'location' and 'env' are supported. 'location' propagates original \
source locations. 'env' inserts additional empty Seq nodes with comments \
relating the Michelson stack to the source LIGO environment." in
  let name = "--michelson-comments" in
  flag ~doc name @@
  listed @@
  Command.Arg_type.create
    (function
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
  let doc  = "force testing mode." in
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

let werror =
  let open Command.Param in
  let name = "--werror" in
  let doc  = "treat warnings as errors" in
  flag ~doc name no_arg

let seed =
  let open Command.Param in
  let name = "--seed" in
  let doc = "SEED the seed or counter used for generation." in
  flag ~doc name @@ optional int

let generator =
  let open Command.Param in
  let name = "generator" in
  let doc = "GEN the generator for mutation." in
  flag ~doc ~aliases:["g"] name @@ optional_with_default Default_options.generator string

let self_pass =
  let open Command.Param in
  let name = "--self-pass" in
  let doc  = "apply the self pass" in
  flag ~doc name no_arg
let dry_run_flag =
  let open Command.Param in
  let name = "--dry-run" in
  let doc  = "don't publish changes to LIGO registry." in
  flag ~doc name no_arg

let project_root =
  let open Command.Param in
  let name = "--project-root" in
  let doc  = "PATH The path to root of the project." in
  let spec = optional string in
  let spec = map_flag spec
    ~f:(function None -> Cli_helpers.find_project_root () | Some x -> Some x) in
  flag ~doc name spec

let cache_path =
  let open Command.Param in
  let name = "--cache-path" in
  let doc  = "PATH The path where dependencies are installed." in
  let spec = optional_with_default Constants.ligo_install_path string in
  flag ~doc name spec

let ligo_registry =
  let open Command.Param in
  let name = "--registry" in
  let doc  = "URL The url to a LIGO registry." in
  let spec = optional_with_default Constants.ligo_registry string in
  flag ~doc name spec

let ligorc_path =
  let open Command.Param in
  let name = "--ligorc-path" in
  let doc  = "PATH path to .ligorc file." in
  let spec = optional_with_default Constants.ligo_rc_path string in
  flag ~doc name spec

let ligoignore_path =
  let open Command.Param in
  let name = "--ligoignore-path" in
  let doc  = "PATH path to .ligoignore file." in
  let spec = optional_with_default Constants.ligo_ignore_path string in
  flag ~doc name spec

let ligo_bin_path =
  let open Command.Param in
  let name = "--ligo-bin-path" in
  let doc  = "PATH path to LIGO executable." in
  let spec = optional_with_default "ligo" string in
  flag ~doc name spec

module Api = Ligo_api
let (<*>) = Command.Param.(<*>)
let (<$>) f a = Command.Param.return f <*> a

(* Command run function of type () -> () and catches exception inside.
I use a mutable variable to propagate back the effect of the result of f *)
let return = ref Done
let reset_return () = return := Done
let compile_file =
  let f source_file entry_point views syntax protocol_version display_format disable_michelson_typechecking experimental_disable_optimizations_for_debugging enable_typed_opt no_stdlib michelson_format output_file show_warnings warning_as_error no_colour michelson_comments constants file_constants project_root warn_unused_rec
        () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~views ~protocol_version ~disable_michelson_typechecking ~experimental_disable_optimizations_for_debugging ~enable_typed_opt ~no_stdlib ~warning_as_error ~no_colour ~constants ~file_constants ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings ?output_file @@
    Api.Compile.contract raw_options source_file display_format michelson_format michelson_comments in
  let summary   = "compile a contract." in
  let readme () = "This sub-command compiles a contract to Michelson \
                  code. It expects a source file and an entrypoint \
                  function that has the type of a contract: \"parameter \
                  * storage -> operations list * storage\"." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> on_chain_views <*> syntax <*> protocol_version <*> display_format <*> disable_michelson_typechecking <*> experimental_disable_optimizations_for_debugging <*> enable_michelson_typed_opt <*> no_stdlib <*> michelson_code_format <*> output_file <*> warn <*> werror <*> no_colour <*> michelson_comments <*> constants <*> file_constants <*> project_root <*> warn_unused_rec)

let compile_parameter =
  let f source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format output_file show_warnings warning_as_error constants file_constants project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~entry_point ~protocol_version ~warning_as_error ~constants ~file_constants ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings ?output_file @@
    Api.Compile.parameter raw_options source_file expression amount balance sender source now display_format michelson_format
  in
  let summary   = "compile parameters to a Michelson expression." in
  let readme () = "This sub-command compiles a parameter for a given \
                  contract to a Michelson expression. The resulting \
                  Michelson expression can be passed as an argument in \
                  a transaction which calls a contract." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> expression "parameter" <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> michelson_code_format <*> output_file <*> warn <*> werror <*> constants <*> file_constants <*> project_root <*> warn_unused_rec)

let compile_expression =
  let f syntax expression protocol_version init_file display_format without_run no_stdlib michelson_format show_warnings warning_as_error constants file_constants project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~without_run ~no_stdlib ~warning_as_error ~constants ~file_constants ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings @@
    Api.Compile.expression raw_options expression init_file display_format michelson_format
    in
  let summary   = "compile to a Michelson value." in
  let readme () = "This sub-command compiles a LIGO expression to a \
                   Michelson value. It works by compiling the LIGO \
                   expression to a Michelson expression and then \
                   interpreting it using Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> expression "" <*> protocol_version <*> init_file <*> display_format  <*> without_run <*> no_stdlib <*> michelson_code_format <*> warn <*> werror <*> constants <*> file_constants <*> project_root <*> warn_unused_rec)

let compile_storage =
  let f source_file expression entry_point syntax protocol_version amount balance sender source now display_format michelson_format output_file show_warnings warning_as_error constants file_constants project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~protocol_version ~warning_as_error ~constants ~file_constants ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings ?output_file @@
    Api.Compile.storage raw_options source_file expression amount balance sender source now display_format michelson_format
  in
  let summary   = "compile an initial storage in LIGO syntax to \
                  a Michelson expression." in
  let readme () = "This sub-command compiles an initial storage for a \
                  given contract to a Michelson expression. The \
                  resulting Michelson expression can be passed as an \
                  argument in a transaction which originates a contract." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "STORAGE" <*> entry_point <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> michelson_code_format <*> output_file <*> warn <*> werror <*> constants <*> file_constants <*> project_root <*> warn_unused_rec)

let compile_constant =
  let f syntax expression protocol_version init_file display_format without_run show_warnings warning_as_error project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~without_run ~warning_as_error ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings @@
    Api.Compile.constant raw_options expression init_file display_format
    in
  let summary   = "compile constant to a Michelson value and its hash." in
  let readme () = "This sub-command compiles a LIGO expression to a \
                   Michelson value and its hash as a global constant. \
                   It works by compiling the LIGO \
                   expression to a Michelson expression and then \
                   interpreting it using Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> expression "" <*> protocol_version <*> init_file <*> display_format  <*> without_run <*> warn <*> werror <*> project_root <*> warn_unused_rec)

let compile_group = Command.group ~summary:"compile a ligo program to michelson" @@
  [ "contract",   compile_file;
    "expression", compile_expression;
    "parameter",  compile_parameter;
    "storage",    compile_storage;
    "constant",   compile_constant;]

(** Transpile commands *)
let transpile_contract =
  let f source_file new_syntax syntax display_format output_file () =
    return_result ~return ?output_file @@
    Api.Transpile.contract source_file new_syntax syntax display_format
  in
  let summary   = "[BETA] transpile a contract to another syntax." in
  let readme () = "[BETA] This sub-command transpiles a source file to another \
                  syntax. It does not use the build system, but the \
                  source file is preprocessed. Comments are currently \
                  not transpiled. Please use at your own risk." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> req_syntax <*> syntax <*> display_format <*> output_file)


let transpile_expression =
  let f syntax expression new_syntax display_format () =
    return_result ~return @@
    Api.Transpile.expression expression new_syntax syntax display_format
  in
  let summary   = "[BETA] transpile an expression to another syntax." in
  let readme () = "[BETA] This sub-command transpiles a LIGO expression to \
                  another syntax. Comments are currently not \
                  transpiled. Please use at your own risk." in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> expression "" <*> req_syntax <*> display_format)

let transpile_group =
  Command.group ~summary:"[BETA] transpile ligo code from a syntax to another" @@
  [ "contract"  , transpile_contract;
    "expression", transpile_expression;]


(** Mutate commands *)
let mutate_cst =
  let f source_file syntax protocol_version libraries display_format seed generator () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~libraries ~generator () in
    return_result ~return @@
    Api.Mutate.mutate_cst raw_options source_file display_format seed in
  let summary   = "return a mutated version for a given file." in
  let readme () = "This sub-command returns a mutated version for a \
                  given file. It does not use the build system." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> libraries <*> display_format <*> seed <*> generator)

let mutate_ast =
  let f source_file syntax protocol_version libraries display_format seed generator () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~libraries ~generator () in
    return_result ~return @@
    Api.Mutate.mutate_ast raw_options source_file display_format seed
  in
  let summary   = "return a mutated version for a given file." in
  let readme () = "This sub-command returns a mutated version for a \
                  given file. It does not use the build system." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> libraries <*> display_format <*> seed <*> generator)

let mutate_group =
  let summary = "create mutants of a ligo file" in
  Command.group ~summary @@
  [ "cst", mutate_cst;
    "ast", mutate_ast;]

(** Run commands *)
let test =
  let f source_file syntax steps cli_expr_inj display_format show_warnings project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~steps ~project_root ~warn_unused_rec ~cli_expr_inj ~test:true () in
    return_result ~return ~show_warnings @@
    Api.Run.test raw_options source_file display_format
  in
  let summary   = "test a contract with the LIGO test framework." in
  let readme () = "This sub-command tests a LIGO contract using a LIGO \
                  interpreter. Still under development, there are features that are work \
                  in progress and are subject to change. No real test \
                  procedure should rely on this sub-command alone."
  in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> steps <*> cli_expr_inj <*> display_format <*> warn <*> project_root <*> warn_unused_rec)

let dry_run =
  let f source_file parameter storage entry_point amount balance sender source now syntax protocol_version display_format show_warnings warning_as_error project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~protocol_version ~warning_as_error ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings @@
    Api.Run.dry_run raw_options source_file parameter storage amount balance sender source now display_format
    in
  let summary   = "run a smart-contract with the given storage and input." in
  let readme () = "This sub-command runs a LIGO contract on a given \
                  storage and parameter. The context is initialized \
                  from a source file where the contract is \
                  implemented. The interpretation is done using \
                  Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "PARAMETER" <*> expression "STORAGE" <*> entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror <*> project_root <*> warn_unused_rec)

let evaluate_call =
  let f source_file parameter entry_point amount balance sender source now syntax protocol_version display_format show_warnings warning_as_error project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~protocol_version ~warning_as_error ~project_root  ~warn_unused_rec() in
    return_result ~return ~show_warnings @@
    Api.Run.evaluate_call raw_options source_file parameter amount balance sender source now display_format
    in
  let summary   = "run a function with the given parameter." in
  let readme () = "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "PARAMETER" <*>  entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror <*> project_root <*> warn_unused_rec)

let evaluate_expr =
  let f source_file entry_point amount balance sender source now syntax protocol_version display_format show_warnings warning_as_error project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~protocol_version ~warning_as_error ~project_root ~warn_unused_rec () in
    return_result ~return ~show_warnings @@
    Api.Run.evaluate_expr raw_options source_file amount balance sender source now display_format
    in
  let summary   = "evaluate a given definition." in
  let readme () = "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror <*> project_root <*> warn_unused_rec)

let interpret =
  let f expression init_file syntax protocol_version amount balance sender source now display_format project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~project_root ~warn_unused_rec () in
    return_result ~return @@
    Api.Run.interpret raw_options expression init_file amount balance sender source now display_format
  in
  let summary   = "interpret the expression in the context initialized by the provided source file." in
  let readme () = "This sub-command interprets a LIGO expression. The \
                  context can be initialized by providing a source \
                  file. The interpretation is done using Michelson's \
                  interpreter." in
  Command.basic ~summary ~readme
  (f <$> expression "EXPRESSION" <*> init_file <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> project_root <*> warn_unused_rec)

let run_group =
  Command.group ~summary:"compile and interpret ligo code"
  [
    "test"         , test;
    "dry-run"      , dry_run;
    "evaluate-call", evaluate_call;
    "evaluate-expr", evaluate_expr;
    "interpret"    , interpret;
  ]

(** Info commands *)
let list_declarations =
  let f source_file only_ep syntax display_format () =
    let raw_options = Raw_options.make ~only_ep ~syntax () in
    return_result ~return @@
    Api.Info.list_declarations raw_options source_file display_format
  in
  let summary   = "list all the top-level declarations." in
  let readme () = "This sub-command prints a list of all top-level \
                  declarations (not including types and modules)." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> only_ep <*> syntax <*> display_format)

let measure_contract =
  let f source_file entry_point views syntax protocol_version display_format enable_typed_opt show_warnings warning_as_error project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~entry_point ~syntax ~protocol_version ~views ~warning_as_error ~project_root ~warn_unused_rec ~enable_typed_opt () in
    return_result ~return ~show_warnings @@
    Api.Info.measure_contract raw_options source_file display_format
  in
  let summary   = "measure a contract's compiled size in bytes." in
  let readme () = "This sub-command compiles a source file and measures \
                  the contract's compiled size in bytes." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> on_chain_views <*> syntax <*> protocol_version <*> display_format <*> enable_michelson_typed_opt <*> warn <*> werror <*> project_root <*> warn_unused_rec)

let get_scope =
  let f source_file protocol_version libraries display_format with_types () =
    let raw_options = Raw_options.make ~protocol_version ~libraries ~with_types () in
    return_result ~return @@
    Api.Info.get_scope raw_options source_file  display_format
  in
  let summary   = "return the JSON encoded environment for a given file." in
  let readme () = "This sub-command returns the environment for a given \
                  file in JSON format. It does not use the build system." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> protocol_version <*> libraries <*> display_format <*> with_types)

let info_group =
  let summary = "tools to get information from contracts" in
  Command.group ~summary
  [ "list-declarations", list_declarations;
    "measure-contract" , measure_contract;
    "get-scope"        , get_scope; ]

(** Print commands *)
let preprocessed =
  let f source_file syntax libraries display_format project_root () =
    let raw_options = Raw_options.make ~syntax ~libraries ~project_root () in
    return_result ~return @@
      Api.Print.preprocess raw_options source_file display_format  in
  let summary   = "preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command runs the pre-processor on a LIGO \
                  source file and outputs the result. The directive \
                  `#include` directly inlines the included file and \
                  therefore its content appears in the output. In \
                  contrast, the directive `#import` includes the file \
                  as a module and therefore the content of the imported \
                  file is not printed by this sub-command." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> libraries <*> display_format <*> project_root)
let pretty_print =
  let f source_file syntax display_format warning_as_error () =
    let raw_options = Raw_options.make ~syntax ~warning_as_error () in
    return_result ~return @@
    Api.Print.pretty_print raw_options source_file display_format in
  let summary   = "pretty-print the source file." in
  let readme () = "This sub-command pretty-prints a source file in \
                  LIGO. The width of the pretty-printed text is \
                  adjusted to the number of columns in the terminal (or \
                  60 if it cannot be determined)." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format <*> werror)
let print_graph =
  let f source_file syntax display_format project_root () =
  let raw_options = Raw_options.make ~syntax ~project_root () in
    return_result ~return @@
    Api.Print.dependency_graph raw_options source_file display_format
  in
  let summary   = "print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the dependency graph created \
                  by the module system. It explores all imported source \
                  files (recursively) following a DFS strategy." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format <*> project_root)

let print_cst =
  let f source_file syntax display_format () =
    let raw_options = Raw_options.make ~syntax () in
    return_result ~return @@
    Api.Print.cst raw_options source_file display_format
  in
  let summary   = "print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the CST \
                  stage, obtained after preprocessing and parsing." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)

let print_ast =
  let f source_file syntax display_format () =
    let raw_options = Raw_options.make ~syntax () in
    return_result ~return @@
    Api.Print.ast raw_options source_file display_format
  in
  let summary   = "print the AST with imperative construct.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  imperative stage, before desugaring step is applied." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)



let print_ast_core =
  let f source_file syntax display_format self_pass project_root () =
    let raw_options = Raw_options.make ~syntax ~self_pass ~project_root () in
    return_result ~return @@
    Api.Print.ast_core raw_options source_file display_format
  in
  let summary  = "print the core ligo AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  core stage." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format <*> self_pass <*> project_root )

let print_ast_typed =
  let f source_file syntax protocol_version display_format self_pass project_root warn_unused_rec test () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~self_pass ~project_root ~warn_unused_rec ~test () in
    return_result ~return @@
    Api.Print.ast_typed raw_options source_file  display_format
  in
  let summary   = "print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  typed stage. Internally, it uses the build system to \
                  type the contract, but the contract is not combined \
                  with imported modules." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> self_pass <*> project_root <*> warn_unused_rec <*> test_mode)

let print_ast_aggregated =
  let f source_file syntax protocol_version display_format self_pass project_root warn_unused_rec test () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~self_pass ~project_root ~warn_unused_rec ~test () in
    return_result ~return @@
      Api.Print.ast_aggregated raw_options source_file display_format
  in
  let summary = "print the contract after aggregation.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                   aggregated stage." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> self_pass <*> project_root <*> warn_unused_rec <*> test_mode)

let print_mini_c =
  let f source_file syntax protocol_version display_format optimize project_root warn_unused_rec () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~project_root ~warn_unused_rec () in
    return_result ~return @@
    Api.Print.mini_c raw_options source_file display_format optimize
  in
  let summary   = "print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the Mini-C \
                  stage. Internally, it uses the build system to type \
                  and compile the contract. Compilation is applied \
                  after combination in the AST typed stage." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> optimize <*> project_root <*> warn_unused_rec)

let print_group =
  let summary = "print intermediary program representation.\nWarning: Intended for development of LIGO and can break at any time" in
  Command.group ~summary ~preserve_subcommand_order:() @@
  [ "preprocessed"    , preprocessed;
    "pretty"          , pretty_print;
    "dependency-graph", print_graph;
    "cst"             , print_cst;
    "ast-imperative"  , print_ast;
    "ast-core"        , print_ast_core;
    "ast-typed"       , print_ast_typed;
    "ast-aggregated"  , print_ast_aggregated;
    "mini-c"          , print_mini_c; ]

(** init *)
let init_library =
  let f project_name template (template_list:bool) display_format () =
    (if template_list then
      return_result ~return @@ Ligo_api.Ligo_init.list ~kind:`LIBRARY ~display_format
    else
      return_result ~return @@ Ligo_api.Ligo_init.new_project ~version:Version.version ~kind:`LIBRARY ~project_name_opt:project_name ~template ~display_format) in
  let summary   = "Generate new folder which contains wished library template" in
  let readme () = "Generate new folder from library template. Internet connexion needed" in
  Command.basic ~summary ~readme (f <$> project_name <*> template <*> template_list <*> display_format)

let init_contract =
  let f project_name template (template_list:bool) display_format () =
    (if template_list then
      return_result ~return @@ Ligo_api.Ligo_init.list ~kind:`CONTRACT ~display_format
    else
      return_result ~return @@ Ligo_api.Ligo_init.new_project ~version:Version.version ~kind:`CONTRACT ~project_name_opt:project_name ~template ~display_format) in
  let summary   = "Generate new folder which contains wished contract template" in
  let readme () = "Generate new folder from contract template. Internet connexion needed" in
  Command.basic ~summary ~readme (f <$> project_name <*> template <*> template_list <*> display_format)

let init_group =
  Command.group ~summary:"Initialize a new ligo project from template. Contract or library."
  [
    "library"       , init_library;
    "contract"      , init_contract;
  ]


(** other *)
let changelog =
  let f display_format () =
    return_result ~return @@ Api.dump_changelog display_format in
  let summary   = "print the ligo changelog" in
  let readme () = "Dump the LIGO changelog to stdout." in
  Command.basic ~summary ~readme
  (f <$> display_format)

let repl =
  let f syntax protocol_version amount balance sender source now display_format init_file project_root () =
    let raw_options = Raw_options.make ~syntax ~protocol_version ~project_root () in
    return_result ~return @@ Repl.main raw_options display_format now amount balance sender source init_file
  in
  let summary   = "interactive ligo interpreter" in
  let readme () = "REPL (Read-Eval-Print-Loop) for LIGO" in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> init_file <*> project_root )

let install =
  let summary   = "install LIGO dependencies declared in package.json" in
  let readme () = "This command invokes the package manager to install the external packages declared in package.json" in
  let f package_name cache_path ligo_registry () =
    return_result ~return @@ fun () -> Install.install ~package_name ~cache_path ~ligo_registry in
  Command.basic ~summary ~readme (f <$> package_name <*> cache_path <*> ligo_registry)

let publish =
  let summary   = "[BETA] publish the LIGO package declared in package.json" in
  let readme () = "[BETA] Packs the pacakage directory contents into a tarball and uploads it to the registry server" in
  let f ligo_registry ligorc_path ligoignore_path project_root dry_run () =
    return_result ~return @@ fun () -> Publish.publish ~ligo_registry ~ligorc_path ~ligoignore_path ~project_root ~dry_run in
  Command.basic ~summary ~readme (f <$> ligo_registry <*> ligorc_path <*> ligoignore_path <*> project_root <*> dry_run_flag)

let add_user =
  let summary   = "[BETA] create a new user for the LIGO package registry" in
  let readme () = "[BETA] Prompt the user for details to create a new user on registry server" in
  let f ligo_registry ligorc_path () =
    return_result ~return @@ fun () -> User.create_or_login ~ligo_registry ~ligorc_path in
  Command.basic ~summary ~readme (f <$> ligo_registry <*> ligorc_path)

let login =
  let summary   = "[BETA] login to the LIGO package registry" in
  let readme () = "[BETA] Prompt the user for credentials to creates a login session with the registry server" in
  let f ligo_registry ligorc_path () =
    return_result ~return @@ fun () -> User.create_or_login ~ligo_registry ~ligorc_path in
  Command.basic ~summary ~readme (f <$> ligo_registry <*> ligorc_path)

let daemon =
  let summary   = "launch a long running LIGO process" in
  let readme () = "Run LIGO subcommands without exiting the process" in
  let f ligo_bin_path () =
    return_result ~return @@ fun () -> Daemon.main ~ligo_bin_path () in
  Command.basic ~summary ~readme (f <$> ligo_bin_path)

let main = Command.group ~preserve_subcommand_order:() ~summary:"The LigoLANG compiler" @@
  [
    "compile"  , compile_group;
    "transpile", transpile_group;
    "run"      , run_group;
    "info"     , info_group;
    "mutate"   , mutate_group;
    "repl"     , repl;
    "init"     , init_group;
    "changelog", changelog;
    "print"    , print_group;
    "install"  , install;
    "publish"  , publish;
    "add-user" , add_user;
    "login"    , login;
    "daemon"   , daemon;
  ]

let run ?argv () =
  let build_info = Format.sprintf "Protocol built-in: %s" Environment.Protocols.(variant_to_string in_use) in
  Command_unix.run ~build_info ~version:Version.version ?argv main;
  (* Effect to error code *)
  match !return with
    Done -> 0;
  | Compileur_Error -> 1;
  | Exception exn ->
    let message msg =
      Format.eprintf "An internal error ocurred. Please, contact the developers.@.";
      if !is_dev then Format.eprintf "%s.@." msg;
      Format.pp_print_flush Format.err_formatter () ;
      2
    in
    match exn with
    | Failure msg -> message msg
    | exn -> message (Exn.to_string exn)
