(** A [Path.t] is expected to be absolute and normalized (normalization is currently
    nontrivial). [UnsafePath] allows to create a [Path.t] from a [string], make sure those
    points are satisfied when using it. *)
type t = UnsafePath of string [@@unboxed] [@@deriving eq, ord, sexp, hash]
(* We don't need to normalize before equality check since we expect `Path.t` to contain
  a path that's already normalized, *)

(** A normalization is a function to turn a file path into a fully canonicalized path.
    Throughout the language server codebase, many functions will take a
    [normalize : normalization] parameter to deal with paths. However, usages of
    [from_absolute] will trigger an alert about performance, as the intended way to
    normalize paths is to either use the [normalize] method from the [lsp_server] class,
    or to use the function provided from [Handler.ask_normalize]. *)
type normalization = string -> t

(** Convert a [Path.t] to a [string]. *)
val to_string : t -> string

(** Create a [Path.t] from a [string] containing an absolute file path. It removes as many
    indirections ([.], [..], symlinks, etc) as possible from the path, returning its
    canonicalized absolute path. *)
val from_absolute : normalization
  [@@alert
    from_absolute_performance
      "This function is expensive. If you will call it in a loop, and such a loop\n\
      \ may have repeated files, prefer binding [Handler.ask_normalize] and calling the\n\
      \ provided function instead. The performance of the language server can be greatly \
       impacted by this."]

(** Create [Path.t] from a string containing file path relative to current dir. Made for
    creating absolute paths in tests. *)
val from_relative : normalization

(** Create a filename which is relative to the base filename. The resulting type is
    [string] since it's expected for [Path.t] to be always absolute. *)
val make_relative : t -> t -> string

(** Returns the directory name that contains this path. *)
val dirname : t -> t

(** Concat absolute path to dir and relative path inside this dir *)
val concat : t -> string -> t

(** Gets the file extension of the given path, if there is one. *)
val get_extension : t -> string option

(** Gets the LIGO syntax of the given path, if there is one. *)
val get_syntax : t -> Syntax_types.t option

(** Searches for a file in a directory and all parental directories. *)
val find_file_in_dir_and_parents : t -> string -> t option

val pp : t Fmt.t
val testable : t Alcotest.testable
