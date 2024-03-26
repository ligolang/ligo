let ligoproject = "ligo.json"

let get_project_root_from_dir : Path.t -> Path.t option =
 fun file ->
  match Path.find_file_in_dir_and_parents file ligoproject with
  | Some project_root -> Some (Path.dirname project_root)
  | None -> None


let get_project_root : Path.t -> Path.t option =
 fun file -> get_project_root_from_dir (Path.dirname file)
