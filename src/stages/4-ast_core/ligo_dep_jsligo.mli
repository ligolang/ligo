(** Folds over the tree, looking for import declarations.
    Returned filenames may be relative to the file of input program. *)
val dependencies : Types.program -> Filename.t list
