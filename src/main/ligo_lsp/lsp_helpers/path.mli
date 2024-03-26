(** [Path.t] is expected to be absolute and normalized (normalization is
    currently nontrivial), [UnsafePath] allows to create a Path form string,
    make sure those points are satisfied when using it. *)
type t = UnsafePath of string [@@unboxed] [@@deriving eq, ord, sexp, hash]
(* We don't need to normalize before equality check since we expect `Path.t` to contain
  a path that's already normalized, *)

val to_string : t -> string

(** Create [Path.t] from a string containing an absolute file path. It removes as many
    indirections ([.], [..], symlinks, etc) as possible from the path, returning its
    canonicalized absolute path. *)
val from_absolute : string -> t
  [@@alert
    from_absolute_performance
      "This function is expensive. If you will call it in a loop, and such a loop\n\
      \ may have repeated files, prefer binding [Handler.ask_normalize] and calling the\n\
      \ provided function instead. The performance of the language server can be greatly \
       impacted by this."]

(** Create [Path.t] from a string containing file path relative to current dir.
    Made for creating absolute paths in tests *)
val from_relative : string -> t

(** Create a filename which is relative to the base filename.
  The resulting type is [string] since it's expected for [Path.t] to be always absolute. *)
val make_relative : t -> t -> string

val dirname : t -> t

(** Concat absolute path to dir and relative path inside this dir *)
val concat : t -> string -> t

val get_extension : t -> string option
val get_syntax : t -> Syntax_types.t option

(** Searches for a file in a directory and all parental directories *)
val find_file_in_dir_and_parents : t -> string -> t option

val pp : t Fmt.t
val testable : t Alcotest.testable
