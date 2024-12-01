(** Folds over the tree, looking for import declarations.
    Returned filenames are relative to the file of input program.
    Filenames are wrapped with locations for better error reporting. *)
val dependencies : Ast_core.program -> Filename.t Simple_utils.Location.wrap list

(** Converts module paths into file paths *)
val imports_of_deps
  :  options:Compiler_options.t
  -> Filename.t
  -> Filename.t Simple_utils.Location.wrap list
  -> (BuildSystem.import * String.t list) list
