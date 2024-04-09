(** The file name where we store LIGO Language Server configurations. The directory that
    contains this file is the project root. *)
let ligoproject = "ligo.json"

(** Given a directory, searches for the innermost project root, if any. E.g., assuming
    that the given path was ["/home/johndoe/example.mligo"], we'll look for the project
    root in ["/home/johndoe"], ["/home"], and ["/"], in this order, stopping at the first
    project root. *)
let get_project_root_from_dir : Path.t -> Path.t option =
 fun file ->
  match Path.find_file_in_dir_and_parents file ligoproject with
  | Some project_root -> Some (Path.dirname project_root)
  | None -> None


(** Given a file (not directory), searches for the innermost project root, if any. E.g.,
    assuming that the given path was ["/home/johndoe/example.mligo"], we'll look for the
    project root in ["/home/johndoe"], ["/home"], and ["/"], in this order, stopping at
    the first project root. *)
let get_project_root : Path.t -> Path.t option =
 fun file -> get_project_root_from_dir (Path.dirname file)
