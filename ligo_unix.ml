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

let rec mkdir_p ~perm dir =
  match Sys.file_exists dir with
  | true -> ()
  | false ->
     let parent = Filename.dirname dir in
     if not (String.equal parent dir) then
       if Sys.file_exists parent then
         Sys.mkdir dir perm
       else
         begin
           mkdir_p ~perm parent;
           Sys.mkdir dir perm
         end
     else ()
