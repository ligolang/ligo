open Cmdliner
open Trace
open Cli_helpers

let version = Version.version

let main =
  let man =
    [ `S "MORE HELP";
      `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
      `S "DOCUMENTATION";
      `P "https://ligolang.org/docs/intro/introduction";
      `S "ASK A QUESTION";
      `P "https://discord.gg/9rhYaEt";
      `S "OPEN AN ISSUE";
      `P "https://gitlab.com/ligolang/ligo/issues/new"
      ]
    in
    (Term.(ret (const (`Help (`Auto, None)))), Term.info "ligo" ~version ~man)

let source_file n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the smart contract file." in
    info ~docv ~doc [] in
  required @@ pos n (some non_dir_file) None info

let entry_point n =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  required @@ pos n (some string) (Some "main") info

let test_entry n =
  let open Arg in
  let info =
    let docv = "TEST_ENTRY" in
    let doc = "$(docv) is top-level variable which will be evaluated as the result of your test." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let expression purpose n =
  let open Arg in
  let docv = purpose ^ "_EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos n (some string) None info

let libraries =
  let open Arg in
  let docv = "LIBRARY" in
  let doc = "$(docv) is a path to a directory containing included files" in
  let info = info ~docv ~doc ["lib" ; "l"] in
  value @@ opt_all string [] info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "auto" info

let protocol_version =
  let open Arg in
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let info =
    let docv = "PROTOCOL_VERSION" in
    let doc = Format.asprintf "$(docv) will decide protocol's types/values pre-loaded into the LIGO environment %s. \
                               By default, the current protocol (%s) will be used" plist (variant_to_string current) in
    info ~docv ~doc ["protocol" ; "p"] in
  value @@ opt string "current" info

let dialect =
  let open Arg in
  let info =
    let docv = "PASCALIGO_DIALECT" in
    let doc = "$(docv) is the pascaligo dialect that will be used. Currently supported dialects are \"terse\" and \"verbose\". By default the dialect is \"terse\"." in
    info ~docv ~doc ["dialect" ; "d"] in
  value @@ opt string "terse" info

let req_syntax n =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively)." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let init_file =
  let open Arg in
  let info =
    let docv = "INIT_FILE" in
    let doc = "$(docv) is the path to smart contract file to be used for context initialization." in
    info ~docv ~doc ["init-file"] in
  value @@ opt (some string) None info

let amount =
  let open Arg in
  let info =
    let docv = "AMOUNT" in
    let doc = "$(docv) is the amount the Michelson interpreter will use for the transaction." in
    info ~docv ~doc ["amount"] in
  value @@ opt string "0" info

let balance =
  let open Arg in
  let info =
    let docv = "BALANCE" in
    let doc = "$(docv) is the balance the Michelson interpreter will use for the contract balance." in
    info ~docv ~doc ["balance"] in
  value @@ opt string "0" info

let sender =
  let open Arg in
  let info =
    let docv = "SENDER" in
    let doc = "$(docv) is the sender the Michelson interpreter transaction will use." in
    info ~docv ~doc ["sender"] in
  value @@ opt (some string) None info

let source =
  let open Arg in
  let info =
    let docv = "SOURCE" in
    let doc = "$(docv) is the source the Michelson interpreter transaction will use." in
    info ~docv ~doc ["source"] in
  value @@ opt (some string) None info

let disable_michelson_typechecking =
  let open Arg in
  let info =
    let doc = "disable Michelson typecking, this might produce ill-typed Michelson code." in
    info ~doc ["disable-michelson-typechecking"] in
  value @@ flag info

let with_types =
  let open Arg in
  let info =
    let doc = "tries to infer types for all named expressions" in
    info ~doc ["with-types"] in
  value @@ flag info

let now =
  let open Arg in
  let info =
    let docv = "NOW" in
    let doc = "$(docv) is the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
    info ~docv ~doc ["now"] in
  value @@ opt (some string) None info

let display_format =
  let open Arg in
  let open Display in
  let info  =
    let docv = "DISPLAY_FORMAT" in
    let doc = "$(docv) is the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
    info ~docv ~doc ["format" ; "display-format"] in
  value @@
  opt
    (enum [("human-readable", human_readable); ("dev", dev); ("json", json)])
    human_readable
    info

let output_file =
  let open Arg in
  let info  =
    let docv = "OUTPUT_FILE" in
    let doc = "$(docv) if used, prints the output into the specified file instead of stdout" in
    info ~docv ~doc ["output" ; "output-file"] in
  value @@ opt (some string) None info

let michelson_code_format =
  let open Arg in
  let info  =
    let docv = "MICHELSON_FORMAT" in
    let doc = "$(docv) is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
    info ~docv ~doc ["michelson-format"] in
  value @@
  opt
    (enum [("text", `Text); ("json", `Json); ("hex", `Hex)])
    `Text info

let optimize =
  let open Arg in
  let docv = "ENTRY_POINT" in
  let doc = "Apply Mini-C optimizations as if compiling $(docv)" in
  let info =
    info ~docv ~doc ["optimize"] in
  value @@ opt (some string) None info

let infer =
  let open Arg in
  let info =
    let doc = "enable type inference" in
    info ~doc ["infer"] in
    value @@ flag info

let warn =
  let open Arg in
  let info =
    let docv = "BOOL" in
    let doc = "$(docv) indicates whether warning messages should be printed in stderr or not" in
    info ~docv ~doc ["warn"] in
    value @@ opt bool true info

let werror =
  let open Arg in
  let info =
    let docv = "BOOL" in
    let doc = "$(docv) indicates whether warning messages should be treated as errors or not" in
    info ~docv ~doc ["werror"] in
    value @@ opt bool false info

module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Run = Run.Of_michelson

let compile_file =
  let f source_file entry_point syntax infer protocol_version display_format disable_typecheck michelson_format output_file warn werror =
    return_result ~werror ~warn ~output_file ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format) @@
      let* options =
        let* init_env = Helpers.get_initial_env protocol_version in
        let* protocol_version = Helpers.protocol_to_variant protocol_version in
        ok @@ Compiler_options.make  ~init_env ~infer ~protocol_version ()
      in
      let* michelson =  Build.build_contract ~options syntax entry_point source_file in
      Compile.Of_michelson.build_contract ~disable_typecheck michelson
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ infer $ protocol_version $ display_format $ disable_michelson_typechecking $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-contract" in
  let doc = "Subcommand: Compile a contract." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a contract to Michelson \
                 code. It expects a source file and an entrypoint \
                 function that has the type of a contract: \"parameter \
                 * storage -> operations list * storage\"."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let preprocess =
  let f source_file syntax display_format =
    return_result ~display_format (Parsing.Formatter.ppx_format) @@
      Trace.map ~f:fst @@
        let options   = Compiler_options.make () in
        let* meta = Compile.Of_source.extract_meta syntax source_file in
        Compile.Of_source.compile ~options ~meta source_file
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "preprocess" in
  let doc = "Subcommand: Preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs the pre-processor on a LIGO \
                 source file and outputs the result. The directive \
                 `#include` directly inlines the included file and \
                 therefore its content appears in the output. In \
                 contrast, the directive `#import` includes the file \
                 as a module and therefore the content of the imported \
                 file is not printed by this sub-command."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let pretty_print =
  let f source_file syntax display_format =
    return_result ~display_format (Parsing.Formatter.ppx_format) @@
        let options = Compiler_options.make () in
        let* meta = Compile.Of_source.extract_meta syntax source_file in
        Compile.Utils.pretty_print ~options ~meta source_file
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "pretty-print" in
  let doc = "Subcommand: Pretty-print the source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command pretty-prints a source file in \
                 LIGO. The width of the pretty-printed text is \
                 adjusted to the number of columns in the terminal (or \
                 60 if it cannot be determined)."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_graph =
  let f source_file syntax display_format =
    return_result ~display_format (Build.Formatter.graph_format) @@
      let options = Compiler_options.make () in
      let* g,_ = Build.dependency_graph ~options syntax Env source_file in
      ok @@ (g,source_file)
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-graph" in
  let doc = "Subcommand: Print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the dependency graph created \
                 by the module system. It explores all imported source \
                 files (recursively) following a DFS strategy."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_cst =
  let f source_file syntax display_format =
    return_result ~display_format (Parsing.Formatter.ppx_format) @@
      let options = Compiler_options.make () in
      let* meta = Compile.Of_source.extract_meta syntax source_file in
      Compile.Utils.pretty_print_cst ~options ~meta source_file
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-cst" in
  let doc = "Subcommand: Print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the CST \
                 stage, obtained after preprocessing and parsing."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast =
  let f source_file syntax display_format =
    return_result ~display_format (Ast_imperative.Formatter.module_format) @@
      let options       = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      Compile.Utils.to_imperative ~options ~meta c_unit source_file
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-ast" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 imperative stage, before sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let print_ast_sugar =
  let f source_file syntax display_format =
    return_result ~display_format (Ast_sugar.Formatter.module_format) @@
      let options = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      Compile.Utils.to_sugar ~options ~meta c_unit source_file
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-ast-sugar" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 stage, after sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_core =
  let f source_file syntax infer protocol_version display_format =
    if infer then
      (* Do the same thing as for print_ast_typed, but only infer the main module
         (it still needs to infer+typecheck the dependencies) *)
      return_result ~display_format (Ast_core.Formatter.module_format) @@
        let* options =
          let* init_env = Helpers.get_initial_env protocol_version in
          ok @@ Compiler_options.make ~infer ~init_env ()
        in
        let* _,inferred_core,_,_ = Build.infer_contract ~options syntax Env source_file in
        ok @@ inferred_core
    else
      (* Print the ast as-is without inferring and typechecking dependencies *)
      return_result ~display_format (Ast_core.Formatter.module_format) @@
        let options = Compiler_options.make ~infer () in
        let* meta     = Compile.Of_source.extract_meta syntax source_file in
        let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
        Compile.Utils.to_core ~options ~meta c_unit source_file
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-core" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 core stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_typed =
  let f source_file syntax infer protocol_version display_format =
    return_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) @@
      let* options =
        let* init_env = Helpers.get_initial_env protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ()
      in
      let* typed,_ = Build.type_contract ~options syntax Env source_file in
      ok @@ typed
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-typed" in
  let doc = "Subcommand: Print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, but the contract is not combined \
                 with imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_combined =
  let f source_file syntax infer protocol_version display_format =
    return_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) @@
      let* options =
        let* init_env = Helpers.get_initial_env protocol_version in
        let* protocol_version = Helpers.protocol_to_variant protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ~protocol_version ()
      in
      let* typed,_ = Build.combined_contract ~options syntax Env source_file in
      ok @@ typed
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-combined" in
  let doc = "Subcommand: Print the contract after combination with the build system.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, and the contract is combined with \
                 the imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_mini_c =
  let f source_file syntax infer protocol_version display_format optimize =
    return_result ~display_format (Mini_c.Formatter.program_format) @@
      let* options =
        let* init_env   = Helpers.get_initial_env protocol_version in
        let* protocol_version = Helpers.protocol_to_variant protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ~protocol_version ()
      in
      let* mini_c,_ = Build.build_mini_c ~options syntax Env source_file in
      match optimize with
        | None -> ok @@ Mini_c.Formatter.Raw mini_c
        | Some entry_point ->
          let* o = Compile.Of_mini_c.aggregate_contract mini_c entry_point in
          ok @@ Mini_c.Formatter.Optimized o
  in
  let term = Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ display_format $ optimize) in
  let cmdname = "print-mini-c" in
  let doc = "Subcommand: Print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the Mini-C \
                 stage. Internally, it uses the build system to type \
                 and compile the contract. Compilation is applied \
                 after combination in the AST typed stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let measure_contract =
  let f source_file entry_point syntax infer protocol_version display_format warn werror =
    return_result ~werror ~warn ~display_format Formatter.contract_size_format @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* michelson =  Build.build_contract ~options syntax entry_point source_file in
      let* contract = Compile.Of_michelson.build_contract michelson in
      Compile.Of_michelson.measure contract
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "measure-contract" in
  let doc = "Subcommand: Measure a contract's compiled size in bytes." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a source file and measures \
                 the contract's compiled size in bytes."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_parameter =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~werror ~warn ~output_file ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format) @@
      let* init_env = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* typed_prg,env   = Build.combined_contract ~options syntax (Contract entry_point) source_file in
      let* mini_c_prg      = Compile.Of_typed.compile typed_prg in
      let* michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg entry_point in
      let* _contract =
       (* fails if the given entry point is not a valid contract *)
        Compile.Of_michelson.build_contract michelson_prg in

      let* typed_param,_    = Compile.Utils.type_expression ~options (Some source_file) syntax expression env in
      let* mini_c_param     = Compile.Of_typed.compile_expression typed_param in
      let* compiled_param   = Compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg mini_c_param in
      let* ()               = Compile.Of_typed.assert_equal_contract_type Check_parameter entry_point typed_prg typed_param in
      let* options          = Run.make_dry_run_options {now ; amount ; balance ; sender;  source ; parameter_ty = None } in
      Run.evaluate_expression ~options compiled_param.expr compiled_param.expr_ty
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-parameter" in
  let doc = "Subcommand: Compile parameters to a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a parameter for a given \
                 contract to a Michelson expression. The resulting \
                 Michelson expression can be passed as an argument in \
                 a transaction which calls a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let interpret =
  let f expression init_file syntax infer protocol_version amount balance sender source now display_format =
    return_result ~display_format (Decompile.Formatter.expression_format) @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let* protocol_version = Helpers.protocol_to_variant protocol_version in
      let options = Compiler_options.make ~infer ~init_env ~protocol_version () in
      let* (decl_list,mods,env) = match init_file with
        | Some init_file ->
           let* mini_c_prg,mods,_,env = Build.build_contract_use ~options syntax init_file in
           ok (mini_c_prg,mods,env)
        | None -> ok ([],Ast_core.SMap.empty,init_env) in
      let* typed_exp,_    = Compile.Utils.type_expression ~options init_file syntax expression env in
      let* mini_c_exp     = Compile.Of_typed.compile_expression ~module_env:mods typed_exp in
      let* compiled_exp   = Compile.Of_mini_c.aggregate_and_compile_expression ~options decl_list mini_c_exp in
      let* options        = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let* runres         = Run.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression typed_exp.type_expression runres
  in
  let term =
    Term.(const f $ expression "EXPRESSION" 0 $ init_file $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format) in
  let cmdname = "interpret" in
  let doc = "Subcommand: Interpret the expression in the context initialized by the provided source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command interprets a LIGO expression. The \
                 context can be initialized by providing a source \
                 file. The interpretation is done using Michelson's \
                 interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_storage =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~werror ~warn ~output_file ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format) @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* typed_prg,env       = Build.combined_contract ~options syntax (Contract entry_point) source_file in
      let* mini_c_prg          = Compile.Of_typed.compile typed_prg in
      let* michelson_prg       = Compile.Of_mini_c.aggregate_and_compile_contract ~options  mini_c_prg entry_point in
      let* _contract =
        (* fails if the given entry point is not a valid contract *)
        Compile.Of_michelson.build_contract michelson_prg in

      let* typed_param,_    = Compile.Utils.type_expression ~options (Some source_file) syntax expression env in
      let* mini_c_param     = Compile.Of_typed.compile_expression typed_param in
      let* compiled_param   = Compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg mini_c_param in
      let* ()               = Compile.Of_typed.assert_equal_contract_type Check_storage entry_point typed_prg typed_param in
      let* options          = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      Run.evaluate_expression ~options compiled_param.expr compiled_param.expr_ty in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-storage" in
  let doc = "Subcommand: Compile an initial storage in LIGO syntax to \
             a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles an initial storage for a \
                 given contract to a Michelson expression. The \
                 resulting Michelson expression can be passed as an \
                 argument in a transaction which originates a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dry_run =
  let f source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~werror ~warn ~display_format (Decompile.Formatter.expression_format) @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* mini_c_prg,_,typed_prg,env = Build.build_contract_use  ~options syntax source_file in
      let* michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg entry_point in
      let* parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let* _contract = Compile.Of_michelson.build_contract michelson_prg in
        match Self_michelson.fetch_contract_inputs michelson_prg.expr_ty with
        | Some (parameter_ty,_storage_ty) -> ok (Some parameter_ty)
        | None -> ok None
      in

      let* compiled_params   = Compile.Utils.compile_storage ~options input storage source_file syntax env mini_c_prg in
      let* args_michelson    = Run.evaluate_expression compiled_params.expr compiled_params.expr_ty in

      let* options           = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty } in
      let* runres  = Run.run_contract ~options michelson_prg.expr michelson_prg.expr_ty args_michelson in
      Decompile.Of_michelson.decompile_typed_program_entry_function_result typed_prg entry_point runres
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "dry-run" in
  let doc = "Subcommand: Run a smart-contract with the given storage and input." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs a LIGO contract on a given \
                 storage and parameter. The context is initialized \
                 from a source file where the contract is \
                 implemented. The interpretation is done using \
                 Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_call ~cmdname_deprecation =
  let f source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~werror ~warn ~display_format (Decompile.Formatter.expression_format) @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* mini_c_prg,mods,typed_prg,env = Build.build_contract_use ~options syntax source_file in
      let* meta             = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit_param,_   = Compile.Of_source.compile_string ~options ~meta parameter in
      let* imperative_param = Compile.Of_c_unit.compile_expression ~meta c_unit_param in
      let* sugar_param      = Compile.Of_imperative.compile_expression imperative_param in
      let* core_param       = Compile.Of_sugar.compile_expression sugar_param in
      let* app              = Compile.Of_core.apply entry_point core_param in
      let* typed_app,_      = Compile.Of_core.compile_expression ~infer ~env app in
      let* compiled_applied = Compile.Of_typed.compile_expression ~module_env:mods typed_app in

      let* michelson        = Compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg compiled_applied in
      let* options          = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let* runres           = Run.run_expression ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_typed_program_entry_function_result typed_prg entry_point runres
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_run_function -> "run-function"
  | `evaluate_call -> "evaluate-call" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_run_function -> "Deprecated, renamed to evaluate-call. Use evaluate-call instead. "
  | `evaluate_call -> "" in
  let doc = deprecation ^ "Subcommand: Run a function with the given parameter." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_expr ~cmdname_deprecation =
  let f source_file entry_point amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~werror ~warn ~display_format Decompile.Formatter.expression_format @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* mini_c,_,typed_prg,_ = Build.build_contract_use ~options syntax source_file in
      let* (exp,_)       = trace_option Main_errors.entrypoint_not_found @@ Mini_c.get_entry mini_c entry_point in
      let exp = Mini_c.e_var ~loc:exp.location (Location.wrap @@ Var.of_name entry_point) exp.type_expression in
      let* compiled      = Compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c exp in
      let* options       = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let* runres        = Run.run_expression ~options compiled.expr compiled.expr_ty in
      Decompile.Of_michelson.decompile_typed_program_entry_expression_result typed_prg entry_point runres
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "evaluate-value"
  | `evaluate_expr -> "evaluate-expr" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "Deprecated, renamed to evaluate-expr. Use evaluate-expr instead. "
  | `evaluate_expr -> "" in
  let doc = deprecation ^ "Subcommand: Evaluate a given definition." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_expression =
  let f expression syntax infer protocol_version init_file display_format michelson_format warn werror =
    return_result ~werror ~warn ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format) @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* (decl_list,env) = match init_file with
        | Some init_file ->
           let* mini_c_prg,env  = Build.build_mini_c ~options syntax Env init_file  in
           ok (mini_c_prg,env)
        | None -> ok ([],init_env) in

      let* typed_exp,_    = Compile.Utils.type_expression ~options init_file syntax expression env in
      let* mini_c_exp     = Compile.Of_typed.compile_expression typed_exp in
      let* compiled_exp   = Compile.Of_mini_c.aggregate_and_compile_expression ~options decl_list mini_c_exp in
      Run.evaluate_expression compiled_exp.expr compiled_exp.expr_ty
    in
  let term =
    Term.(const f $ expression "" 1 $ req_syntax 0 $ infer $ protocol_version $ init_file $ display_format $ michelson_code_format $ warn $ werror) in
  let cmdname = "compile-expression" in
  let doc = "Subcommand: Compile to a Michelson value." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a LIGO expression to a \
                 Michelson value. It works by compiling the LIGO \
                 expression to a Michelson expression and then \
                 interpreting it using Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dump_changelog =
  let f display_format =
    let value = Changelog.changelog in
    let format = Formatter.changelog_format in
    toplevel ~display_format (Display.Displayable {value ; format}) (ok value) in
  let term =
    Term.(const f $ display_format) in
  let cmdname = "changelog" in
  let doc = "Dump the LIGO changelog to stdout." in
  let man = [`S Manpage.s_description;
             `P "This sub-command dumps the changelog to the stdout."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let list_declarations =
  let f source_file syntax display_format =
    return_result ~display_format Formatter.declarations_format @@
      let options       = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core_prg = Compile.Utils.to_core ~options ~meta c_unit source_file in
      let declarations  = Compile.Of_core.list_declarations core_prg in
      ok (source_file, declarations)
  in
  let term =
    Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "list-declarations" in
  let doc = "Subcommand: List all the top-level declarations." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints a list of all top-level \
                 declarations (not including types and modules)."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_contract =
  let f source_file new_syntax syntax new_dialect display_format output_file =
    return_result ~output_file ~display_format (Parsing.Formatter.ppx_format) @@
      let options         = Compiler_options.make () in
      let* meta       = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_   = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core       = Compile.Utils.to_core ~options ~meta c_unit source_file in
      let* sugar      = Decompile.Of_core.decompile core in
      let* imperative = Decompile.Of_sugar.decompile sugar in
      let dialect         = Decompile.Helpers.Dialect_name new_dialect in
      let* buffer     =
        Decompile.Of_imperative.decompile ~dialect imperative (Syntax_name new_syntax) in
      ok @@ buffer
  in
  let term =
    Term.(const f $ source_file 0 $ req_syntax 1  $ syntax $ dialect $ display_format $ output_file) in
  let cmdname = "transpile-contract" in
  let doc = "Subcommand: Transpile a contract to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a source file to another \
                 syntax. It does not use the build system, but the \
                 source file is preprocessed. Comments are currently \
                 not transpiled. Please use at your own risk."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_expression =
  let f expression new_syntax syntax new_dialect display_format =
    return_result ~display_format (Parsing.Formatter.ppx_format) @@
      (* Compiling chain *)
      let options            = Compiler_options.make () in
      let* meta          = Compile.Of_source.make_meta syntax None in
      let* c_unit_expr,_ = Compile.Of_source.compile_string ~options ~meta expression in
      let* imperative    = Compile.Of_c_unit.compile_expression ~meta c_unit_expr in
      let* sugar         = Compile.Of_imperative.compile_expression imperative in
      let* core          = Compile.Of_sugar.compile_expression sugar in
      (* Decompiling chain *)
      let      dialect       = Decompile.Helpers.Dialect_name new_dialect in
      let* n_syntax      = Decompile.Helpers.syntax_to_variant ~dialect (Syntax_name new_syntax) None in
      let* sugar         = Decompile.Of_core.decompile_expression core in
      let* imperative    = Decompile.Of_sugar.decompile_expression sugar in
      let* buffer        = Decompile.Of_imperative.decompile_expression imperative n_syntax in
      ok @@ buffer
  in
  let term =
    Term.(const f $ expression "" 1  $ req_syntax 2 $ req_syntax 0 $ dialect $ display_format) in
  let cmdname = "transpile-expression" in
  let doc = "Subcommand: Transpile an expression to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a LIGO expression to \
                 another syntax. Comments are currently not \
                 transpiled. Please use at your own risk."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let get_scope =
  let f source_file syntax infer protocol_version libs display_format with_types =
    return_result ~display_format Scopes.Formatter.scope_format @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options       = Compiler_options.make ~infer ~init_env ~libs () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core_prg = Compile.Utils.to_core ~options ~meta c_unit source_file in
      Scopes.scopes ~with_types ~options core_prg
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ libraries $ display_format $ with_types) in
  let cmdname = "get-scope" in
  let doc = "Subcommand: Return the JSON encoded environment for a given file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command returns the environment for a given \
                 file in JSON format. It does not use the build system."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let test =
  let f source_file test_entry syntax infer protocol_version display_format =
    return_result ~display_format (Ligo_interpreter.Formatter.test_format) @@
      let* init_env   = Helpers.get_initial_env ~test_env:true protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* typed,_    = Compile.Utils.type_file ~options source_file syntax Env in
      Interpreter.eval_test typed test_entry
  in
  let term =
    Term.(const f $ source_file 0 $ test_entry 1 $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "test" in
  let doc = "Subcommand: Test a contract with the LIGO test framework (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command tests a LIGO contract using a LIGO \
                 interpreter, no Michelson code is evaluated. Still \
                 under development, there are features that are work \
                 in progress and are subject to change. No real test \
                 procedure should rely on this sub-command alone.";
             (* 
             TODO: correct text below
             
             `S "EXTRA PRIMITIVES FOR TESTING";
             `P "Test.originate c st : binds contract c with the \
                 address addr which is returned, st as the initial \
                 storage.";
             `P "Test.set_now t : sets the current time to t.";
             `P "Test.set_balance addr b : sets the balance of \
                 contract bound to address addr (returns unit).";
             `P "Test.external_call addr p amt : performs a call to \
                 contract bound to addr with parameter p and amount \
                 amt (returns unit).";
             `P "Test.get_storage addr : returns current storage bound \
                 to address addr.";
             `P "Test.get_balance : returns current balance bound to \
                 address addr.";
             `P "Test.assert_failure (f : unit -> _) : returns true if \
                 f () fails.";
             `P "Test.log x : prints x into the console." *)
            ]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let repl =
  let f syntax_name protocol_version infer
    amount balance sender source now display_format init_file : unit Term.ret =
    (let protocol = Environment.Protocols.protocols_to_variant protocol_version in
    let syntax = Helpers.syntax_to_variant (Syntax_name syntax_name) None in
    let dry_run_opts = Run.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
    match protocol, Trace.to_option syntax, Trace.to_option dry_run_opts with
    | _, None, _ -> `Error (false, "Please check syntax name.")
    | None, _, _ -> `Error (false, "Please check protocol name.")
    | _, _, None -> `Error (false, "Please check run options.")
    | Some protocol, Some syntax, Some dry_run_opts ->
       `Ok (Repl.main syntax display_format protocol infer dry_run_opts init_file)) in
  let term =
    Term.(const f $ req_syntax 0 $ protocol_version $ infer $ amount $ balance $ sender $ source $ now $ display_format $ init_file) in
  let cmdname = "repl" in
  let doc = "Subcommand: REPL" in
  (Term.ret term , Term.info ~doc cmdname)


let buffer = Buffer.create 100


let run ?argv () =
  let err = Format.formatter_of_buffer buffer in
  Term.eval_choice ~err ?argv main [
    test ;
    compile_file ;
    measure_contract ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    transpile_contract ;
    transpile_expression ;
    interpret ;
    dry_run ;
    evaluate_call ~cmdname_deprecation:`deprecated_run_function ;
    evaluate_call ~cmdname_deprecation:`evaluate_call ;
    evaluate_expr ~cmdname_deprecation:`deprecated_evaluate_value ;
    evaluate_expr ~cmdname_deprecation:`evaluate_expr ;
    dump_changelog ;
    print_graph ;
    print_cst ;
    print_ast ;
    print_ast_sugar ;
    print_ast_core ;
    print_ast_typed ;
    print_ast_combined ;
    print_mini_c ;
    list_declarations ;
    preprocess;
    pretty_print;
    get_scope;
    repl;
  ]
