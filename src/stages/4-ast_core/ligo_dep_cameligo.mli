(** Folds over the tree, looking for external modules. *)
val dependencies : std_lib:Types.module_ -> Types.program -> String.t list
