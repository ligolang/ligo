include Unix
let ls_dir directory = Array.to_list (Sys.readdir directory)
let home_directory () =
  match Sys.getenv_opt "HOME" with
  | Some home -> home
  | None when Sys.win32-> (match Sys.getenv_opt "USERPROFILE", Sys.getenv "HOMEDRIVE", Sys.getenv "HOMEPATH" with
                          | _, home_drive, home_path -> home_drive ^ home_path
                          | Some home, _, _ -> home
                          | _ -> "")
  | None -> ""
