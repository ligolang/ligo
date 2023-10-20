let ligoproject = "ligo.json"

let get_project_root : Path.t -> Path.t option =
 fun file ->
  match Path.find_file_in_dir_and_parents (Path.dirname file) ligoproject with
  | Some project_root -> Some (Path.dirname project_root)
  | None -> None
