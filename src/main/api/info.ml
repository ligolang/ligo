open Api_helpers
open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let measure_contract source_file entry_point syntax infer protocol_version display_format werror =
    format_result ~werror ~display_format Formatter.contract_size_format @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options = Compiler_options.make ~infer ~init_env () in
      let* michelson =  Build.build_contract ~options syntax entry_point source_file in
      let* contract = Compile.Of_michelson.build_contract michelson in
      Compile.Of_michelson.measure contract

let list_declarations source_file syntax display_format =
    format_result ~display_format Formatter.declarations_format @@
      let options       = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core_prg = Compile.Utils.to_core ~options ~meta c_unit source_file in
      let declarations  = Compile.Of_core.list_declarations core_prg in
      ok (source_file, declarations)

let get_scope source_file syntax infer protocol_version libs display_format with_types =
    format_result ~display_format Scopes.Formatter.scope_format @@
      let* init_env   = Helpers.get_initial_env protocol_version in
      let options       = Compiler_options.make ~infer ~init_env ~libs () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core_prg = Compile.Utils.to_core ~options ~meta c_unit source_file in
      Scopes.scopes ~with_types ~options core_prg
