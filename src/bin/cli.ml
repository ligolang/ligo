open Cli_helpers

let is_dev = ref true

let entry_point =
  let open Command.Param in
  let name = "e" in
  let doc = "ENTRY-POINT the entry-point that will be compiled." in
  let spec = optional_with_default "main" string in
  flag ~doc ~aliases:["--entry-point"] name spec

let source_file =
    let name = "SOURCE_FILE" in
    let _doc = "the path to the smart contract file." in
    Command.Param.(anon (name %: Filename.arg_type))

let expression purpose =
  let name = purpose ^ "_EXPRESSION" in
  let _desc = "the expression that will be compiled." in
  Command.Param.(anon (name %: string))

let libraries : string list Command.Param.t =
  let open Command.Param in
  let name = "--library" in
  let doc  = "LIBS A comma-separated list of paths to directories where to search for files to be included by the preprocessor" in
  let spec = optional_with_default [] @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string in
  flag ~doc ~aliases:["l"] name spec

let syntax =
  let open Command.Param in
  let doc  = "SYNTAX the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
  let spec = optional_with_default "auto" string in
  flag ~doc ~aliases:["s"] "--syntax" spec

let on_chain_views : _ Command.Param.t =
  let open Command.Param in
  let doc  = "VIEWS A list of declaration name that will be compiled as on-chain views, separated by ','" in
  let spec = optional_with_default [] @@ Command.Arg_type.comma_separated ~strip_whitespace:true ~unique_values:true string in
  flag ~doc ~aliases:["v"] "--views" spec

let steps =
  let open Command.Param in
  let doc  = "INT a bound in the number of steps to be done by the interpreter." in
  let spec = optional_with_default 1000000 int in
  flag ~doc ~aliases:["n"] "--steps" spec

let protocol_version =
  let open Command.Param in
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let doc   = Format.asprintf "PROTOCOL choose protocol's types/values pre-loaded into the LIGO environment %s. \
                              By default, the current protocol (%s) will be used" plist (variant_to_string current) in
  let spec  = optional_with_default "current" string in
  flag ~doc ~aliases:["--protocol"] "p" spec

let dialect =
  let open Command.Param in
  let name = "--pascaligo-dialect" in
  let doc  = "DIALECT the pascaligo dialect that will be used. Currently supported dialects are \"terse\" and \"verbose\". By default the dialect is \"terse\"." in
  let spec = optional_with_default "terse" string in
  flag ~doc ~aliases:["d";"dialect"] name spec

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

let without_run =
  let open Command.Param in
  let name = "--without-run" in
  let doc  = "disable running of compiled expression." in
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

let display_format =
  let open Command.Param in
  let open Simple_utils.Display in
  let name = "--display-format" in
  let doc  = "format the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
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
  let doc = "format is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
  flag ~doc docv @@
  optional_with_default `Text @@
  Command.Arg_type.create @@ function
    | "text" -> `Text
    | "json" -> `Json
    | "hex"  -> `Hex
    | _ -> failwith "todo"

let michelson_comments =
  let open Command.Param in
  let doc = "Selects kinds of comments to be added to the Michelson output. Currently only 'location' is supported, which propagates original source locations (line/col)." in
  let name = "--michelson-comments" in
  flag ~doc name @@
  listed @@
  Command.Arg_type.create @@ function
  (* autocomplete:(fun _ -> return ["location"]) *)
  | "location" -> `Location
  | s -> failwithf "unexpected value for --%s: %s" name s ()

let optimize =
  let open Command.Param in
  let name = "--optimize" in
  let doc = "ENTRY_POINT Apply Mini-C optimizations as if compiling ENTRY_POINT" in
  flag ~doc name @@ optional string

let warn =
  let open Command.Param in
  let name = "--no-warn" in
  let doc = "disable warning messages" in
  map ~f:not @@ flag ~doc name no_arg

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
  flag ~doc ~aliases:["g"] name @@ optional_with_default "random" string

let self_pass =
  let open Command.Param in
  let name = "--self-pass" in
  let doc  = "apply the self pass" in
  flag ~doc name no_arg

module Api = Ligo_api
let (<*>) = Command.Param.(<*>)
let (<$>) f a = Command.Param.return f <*> a

(* Command run function of type () -> () and catches exception inside.
I use a mutable variable to propagate back the effect of the result of f *)
let return = ref Done
let compile_file =
  let f source_file entry_point oc_views syntax protocol_version display_format disable_typecheck michelson_format output_file warn werror michelson_comments () =
    return_result ~return ~warn ?output_file @@
    Api.Compile.contract ~werror source_file entry_point oc_views syntax protocol_version display_format disable_typecheck michelson_format michelson_comments in
  let summary   = "compile a contract." in
  let readme () = "This sub-command compiles a contract to Michelson \
                  code. It expects a source file and an entrypoint \
                  function that has the type of a contract: \"parameter \
                  * storage -> operations list * storage\"." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> on_chain_views <*> syntax <*> protocol_version <*> display_format <*> disable_michelson_typechecking <*> michelson_code_format <*> output_file <*> warn <*> werror <*> michelson_comments)


let compile_parameter =
  let f source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format output_file warn werror () =
    return_result ~return ~warn ?output_file @@
    Api.Compile.parameter source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format werror
  in
  let summary   = "compile parameters to a Michelson expression." in
  let readme () = "This sub-command compiles a parameter for a given \
                  contract to a Michelson expression. The resulting \
                  Michelson expression can be passed as an argument in \
                  a transaction which calls a contract." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> expression "parameter" <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> michelson_code_format <*> output_file <*> warn <*> werror)

let compile_expression =
  let f syntax expression protocol_version init_file display_format without_run michelson_format warn werror () =
    return_result ~return ~warn @@
    Api.Compile.expression expression syntax protocol_version init_file display_format without_run michelson_format werror
    in
  let summary   = "compile to a Michelson value." in
  let readme () = "This sub-command compiles a LIGO expression to a \
                   Michelson value. It works by compiling the LIGO \
                   expression to a Michelson expression and then \
                   interpreting it using Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> expression "" <*> protocol_version <*> init_file <*> display_format  <*> without_run <*> michelson_code_format <*> warn <*> werror)

let compile_storage =
  let f source_file expression entry_point syntax protocol_version amount balance sender source now display_format michelson_format output_file warn werror () =
    return_result ~return ~warn ?output_file @@
    Api.Compile.storage source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format werror
  in
  let summary   = "compile an initial storage in LIGO syntax to \
                  a Michelson expression." in
  let readme () = "This sub-command compiles an initial storage for a \
                  given contract to a Michelson expression. The \
                  resulting Michelson expression can be passed as an \
                  argument in a transaction which originates a contract." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "STORAGE" <*> entry_point <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> michelson_code_format <*> output_file <*> warn <*> werror)

let compile_group = Command.group ~summary:"compile a ligo program to michelson" @@
  [ "contract",   compile_file;
    "expression", compile_expression;
    "parameter",  compile_parameter;
    "storage",    compile_storage;]

(** Transpile commands *)
let transpile_contract =
  let f source_file new_syntax syntax new_dialect display_format output_file () =
    return_result ~return ?output_file @@
    Api.Transpile.contract source_file new_syntax syntax new_dialect display_format
  in
  let summary   = "transpile a contract to another syntax (BETA)." in
  let readme () = "This sub-command transpiles a source file to another \
                  syntax. It does not use the build system, but the \
                  source file is preprocessed. Comments are currently \
                  not transpiled. Please use at your own risk." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> req_syntax <*> syntax <*> dialect <*> display_format <*> output_file)


let transpile_expression =
  let f syntax expression new_syntax new_dialect display_format () =
    return_result ~return @@
    Api.Transpile.expression expression new_syntax syntax new_dialect display_format
  in
  let summary   = "transpile an expression to another syntax (BETA)." in
  let readme () = "This sub-command transpiles a LIGO expression to \
                  another syntax. Comments are currently not \
                  transpiled. Please use at your own risk." in
  Command.basic ~summary ~readme
  (f <$> req_syntax <*> expression "" <*> req_syntax <*> dialect <*> display_format)

let transpile_group =
  Command.group ~summary:"transpile ligo code from a syntax to another (BETA)" @@
  [ "contract"  , transpile_contract;
    "expression", transpile_expression;]


(** Mutate commands *)
let mutate_cst =
  let f source_file syntax protocol_version libs display_format seed generator () =
    return_result ~return @@
    Api.Mutate.mutate_cst source_file syntax protocol_version libs display_format seed generator in
  let summary   = "return a mutated version for a given file." in
  let readme () = "This sub-command returns a mutated version for a \
                  given file. It does not use the build system." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> libraries <*> display_format <*> seed <*> generator)

let mutate_ast =
  let f source_file syntax protocol_version libs display_format seed generator () =
    return_result ~return @@
    Api.Mutate.mutate_ast source_file syntax protocol_version libs display_format seed generator
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
  let f source_file syntax steps protocol_version display_format () =
    return_result ~return @@
    Api.Run.test source_file syntax steps protocol_version display_format
  in
  let summary   = "test a contract with the LIGO test framework (BETA)." in
  let readme () = "This sub-command tests a LIGO contract using a LIGO \
                  interpreter. Still under development, there are features that are work \
                  in progress and are subject to change. No real test \
                  procedure should rely on this sub-command alone."
  in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> steps <*> protocol_version <*> display_format)

let dry_run =
  let f source_file parameter storage entry_point amount balance sender source now syntax protocol_version display_format warn werror () =
    return_result ~return ~warn @@
    Api.Run.dry_run source_file entry_point parameter storage amount balance sender source now syntax protocol_version display_format werror
    in
  let summary   = "run a smart-contract with the given storage and input." in
  let readme () = "This sub-command runs a LIGO contract on a given \
                  storage and parameter. The context is initialized \
                  from a source file where the contract is \
                  implemented. The interpretation is done using \
                  Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "PARAMETER" <*> expression "STORAGE" <*> entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror)

let evaluate_call =
  let f source_file parameter entry_point amount balance sender source now syntax protocol_version display_format warn werror () =
    return_result ~return ~warn @@
    Api.Run.evaluate_call source_file entry_point parameter amount balance sender source now syntax protocol_version display_format werror
    in
  let summary   = "run a function with the given parameter." in
  let readme () = "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> expression "PARAMETER" <*>  entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror)

let evaluate_expr =
  let f source_file entry_point amount balance sender source now syntax protocol_version display_format warn werror () =
    return_result ~return ~warn @@
    Api.Run.evaluate_expr source_file entry_point amount balance sender source now syntax protocol_version display_format werror
    in
  let summary   = "evaluate a given definition." in
  let readme () = "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> amount <*> balance <*> sender <*> source <*> now <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror)

let interpret =
  let f expression init_file syntax protocol_version amount balance sender source now display_format () =
    return_result ~return @@
    Api.Run.interpret expression init_file syntax protocol_version amount balance sender source now display_format
  in
  let summary   = "interpret the expression in the context initialized by the provided source file." in
  let readme () = "This sub-command interprets a LIGO expression. The \
                  context can be initialized by providing a source \
                  file. The interpretation is done using Michelson's \
                  interpreter." in
  Command.basic ~summary ~readme
  (f <$> expression "EXPRESSION" <*> init_file <*> syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format)

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
  let f source_file syntax display_format () =
    return_result ~return @@
    Api.Info.list_declarations source_file syntax display_format
  in
  let summary   = "list all the top-level declarations." in
  let readme () = "This sub-command prints a list of all top-level \
                  declarations (not including types and modules)." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> display_format)

let measure_contract =
  let f source_file entry_point oc_views syntax protocol_version display_format warn werror () =
    return_result ~return ~warn @@
    Api.Info.measure_contract source_file entry_point oc_views syntax protocol_version display_format werror
  in
  let summary   = "measure a contract's compiled size in bytes." in
  let readme () = "This sub-command compiles a source file and measures \
                  the contract's compiled size in bytes." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> entry_point <*> on_chain_views <*> syntax <*> protocol_version <*> display_format <*> warn <*> werror)

let get_scope =
  let f source_file protocol_version libs display_format with_types () =
    return_result ~return @@
    Api.Info.get_scope source_file protocol_version libs display_format with_types
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
  let f source_file syntax libraries display_format () =
    return_result ~return @@
      Api.Print.preprocess source_file syntax libraries display_format in
  let summary   = "preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command runs the pre-processor on a LIGO \
                  source file and outputs the result. The directive \
                  `#include` directly inlines the included file and \
                  therefore its content appears in the output. In \
                  contrast, the directive `#import` includes the file \
                  as a module and therefore the content of the imported \
                  file is not printed by this sub-command." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> libraries <*> display_format)
let pretty_print =
  let f source_file syntax display_format () =
    return_result ~return @@
    Api.Print.pretty_print source_file syntax display_format in
  let summary   = "pretty-print the source file." in
  let readme () = "This sub-command pretty-prints a source file in \
                  LIGO. The width of the pretty-printed text is \
                  adjusted to the number of columns in the terminal (or \
                  60 if it cannot be determined)." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)
let print_graph =
  let f source_file syntax display_format () =
    return_result ~return @@
    Api.Print.dependency_graph source_file syntax display_format
  in
  let summary   = "print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the dependency graph created \
                  by the module system. It explores all imported source \
                  files (recursively) following a DFS strategy." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)

let print_cst =
  let f source_file syntax display_format () =
    return_result ~return @@
    Api.Print.cst source_file syntax display_format
  in
  let _cmdname = "print-cst" in
  let summary   = "print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the CST \
                  stage, obtained after preprocessing and parsing." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)

let print_ast =
  let f source_file syntax display_format () =
    return_result ~return@@
    Api.Print.ast source_file syntax display_format
  in
  let summary   = "print the AST with imperative construct.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  imperative stage, before desugaring step is applied." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format)


let print_ast_sugar =
  let f source_file syntax display_format self_pass () =
    return_result ~return @@
    Api.Print.ast_sugar source_file syntax display_format self_pass
  in
  let summary   = "print the AST with syntatic sugar.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  stage, after desugaring step is applied." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format <*> self_pass)

let print_ast_core =
  let f source_file syntax display_format self_pass () =
    return_result ~return @@
    Api.Print.ast_core source_file syntax display_format self_pass
  in
  let summary  = "print the core ligo AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  core stage." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> display_format <*> self_pass)

let print_ast_typed =
  let f source_file syntax protocol_version display_format self_pass () =
    return_result ~return @@
    Api.Print.ast_typed source_file syntax protocol_version display_format self_pass
  in
  let summary   = "print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  typed stage. Internally, it uses the build system to \
                  type the contract, but the contract is not combined \
                  with imported modules." in
  Command.basic ~summary ~readme @@
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> self_pass)

let print_ast_aggregated =
  let f source_file syntax protocol_version display_format self_pass () =
    return_result ~return @@
      Api.Print.ast_aggregated source_file syntax protocol_version display_format self_pass
  in
  let summary = "print the contract after aggregation.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                   aggregated stage." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> self_pass)

let print_ast_combined =
  let f source_file syntax protocol_version display_format () =
    return_result ~return @@
    Api.Print.ast_combined source_file syntax protocol_version display_format
  in
  let summary   = "print the contract after combination with the build system.\n Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the AST \
                  typed stage. Internally, it uses the build system to \
                  type the contract, and the contract is combined with \
                  the imported modules." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format)

let print_mini_c =
  let f source_file syntax protocol_version display_format optimize () =
    return_result ~return @@
    Api.Print.mini_c source_file syntax protocol_version display_format optimize
  in
  let summary   = "print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let readme () = "This sub-command prints the source file in the Mini-C \
                  stage. Internally, it uses the build system to type \
                  and compile the contract. Compilation is applied \
                  after combination in the AST typed stage." in
  Command.basic ~summary ~readme
  (f <$> source_file <*> syntax <*> protocol_version <*> display_format <*> optimize)

let print_group =
  let summary = "print intermediary program representation.\nWarning: Intended for development of LIGO and can break at any time" in
  Command.group ~summary ~preserve_subcommand_order:() @@
  [ "preprocessed"    , preprocessed;
    "pretty"          , pretty_print;
    "dependency-graph", print_graph;
    "cst"             , print_cst;
    "ast-imperative"  , print_ast;
    "ast-sugar"       , print_ast_sugar;
    "ast-core"        , print_ast_core;
    "ast-typed"       , print_ast_typed;
    "ast-combined"    , print_ast_combined;
    "ast-aggregated"  , print_ast_aggregated;
    "mini-c"          , print_mini_c; ]

(** other *)
let changelog =
  let f display_format () =
    return_result ~return @@ Api.dump_changelog display_format in
  let summary   = "print the ligo changelog" in
  let readme () = "Dump the LIGO changelog to stdout." in
  Command.basic ~summary ~readme
  (f <$> display_format)

let repl =
  let f syntax_name protocol_version amount balance sender source now display_format init_file () =
     return_result ~return @@ fun () ->
    (let protocol = Environment.Protocols.protocols_to_variant protocol_version in
    let syntax = Ligo_compile.Helpers.syntax_to_variant (Syntax_name syntax_name) None in
    let dry_run_opts = Ligo_run.Of_michelson.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
    match protocol, Simple_utils.Trace.to_option syntax, Simple_utils.Trace.to_option dry_run_opts with
    | _, None, _ -> Error ("", "Please check syntax name.")
    | None, _, _ -> Error ("", "Please check protocol name.")
    | _, _, None -> Error ("", "Please check run options.")
    | Some protocol, Some syntax, Some dry_run_opts ->
       (Repl.main syntax display_format protocol dry_run_opts init_file); Ok("","")) in
  let summary   = "interactive ligo interpreter" in
  let _readme () = "" in
  Command.basic ~summary
  (f <$> req_syntax <*> protocol_version <*> amount <*> balance <*> sender <*> source <*> now <*> display_format <*> init_file)

let main = Command.group ~preserve_subcommand_order:() ~summary:"the LigoLANG compiler" @@
  [
    "compile"  , compile_group;
    "transpile", transpile_group;
    "run"      , run_group;
    "info"     , info_group;
    "mutate"   , mutate_group;
    "repl"     , repl;
    "changelog", changelog;
    "print"    , print_group;
  ]

let run ?argv () =
  Command.run ~version:Version.version ?argv main;
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
