(** Folds over the tree, looking for external modules.
    Module names are wrapped with locations for better error reporting. *)
val dependencies : std_lib:Ast_typed.module_ -> Ast_core.program -> String.t Simple_utils.Location.wrap list
