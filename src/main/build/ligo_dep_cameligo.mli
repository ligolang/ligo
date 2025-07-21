(** Folds over the tree, looking for external modules.
    Module paths are wrapped with locations for better error reporting. *)
val dependencies
  :  std_lib:Ast_typed.module_
  -> Ast_core.program
  -> String.t list Simple_utils.Location.wrap list

(** Converts module paths into file paths *)
val imports_of_deps
  :  options:Compiler_options.t
  -> Filename.t
  -> String.t list Simple_utils.Location.wrap list
  -> (BuildSystem.import * String.t list) list
