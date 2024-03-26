open Lsp_helpers

(** Resolves provided string path against [lsp_test] directory.
    This function assumes that the given path points to some file in [src/test]. *)
let resolve : string -> string = FilePath.concat "../"

(** Makes an absolute path from some path that is relative to [src/test]. *)
let normalize_path : string -> Path.t = Path.from_relative <@ resolve

(** Makes the given string path relative to your cwd.
    Helpful for tests where we don't want to see hardcoded absolute paths. *)
let string_path_to_relative : string -> string =
  FilePath.make_relative (Filename.dirname @@ Sys_unix.getcwd ())


(** The same as above but for [Path.t]. *)
let path_to_relative : Path.t -> string = string_path_to_relative <@ Path.to_string

(** Makes path in [DocumentUri.t] relative to your cwd. *)
let to_relative_uri ~(normalize : string -> Path.t) (uri : DocumentUri.t) : DocumentUri.t =
  let abs_path = DocumentUri.to_path ~normalize uri in
  let rel_path = path_to_relative abs_path in
  DocumentUri.of_path (normalize rel_path)


(** Runs a bunch of expect tests and prints the result as a list. *)
let run_multiple_tests : 'a list -> test_runner:('a -> unit) -> unit =
 fun tests ~test_runner ->
  Format.printf "%a" (Fmt.Dump.list @@ fun _ -> test_runner) tests
