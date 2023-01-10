include Unix
let ls_dir directory = Array.to_list (Sys.readdir directory)
let home_directory () =
  match Sys.getenv_opt "HOME" with
  | Some home -> home
  | None when Sys.win32-> (match Sys.getenv_opt "USERPROFILE", Sys.getenv_opt "HOMEDRIVE", Sys.getenv_opt "HOMEPATH" with
                          | _, Some home_drive, Some home_path -> home_drive ^ home_path
                          | Some home, _, _ -> home
                          | _, _, _ -> "")
  | None -> ""

let putenv ~key ~data = putenv key data
let mkdir ~perm dir = mkdir dir perm

let mkdir_p ~perm dir =
  let rec dirs dir =
    if dir = Filename.dirname dir
    then []
    else Filename.basename dir :: dirs (Filename.dirname dir)
  in
  let dirs = dirs dir in
  let _ =
    List.fold_right
      (fun dir dirs ->
        let dir = if dir = String.empty then dir else Filename.concat dirs dir in
        let () =
          try mkdir ~perm dir with
          | Unix_error ((EEXIST | EISDIR), _, _) -> ()
        in
        dir)
      dirs
      String.empty
  in
  ()