type command = string * string array

let ligo_install_path = "./.ligo"
let ligo_rc_path () = Filename.concat (Ligo_unix.home_directory ()) ".ligorc"
let ligo_registry = "https://packages.ligolang.org"
let esy = "esy"
let windows = "Win32"
let typedoc = "typedoc"

let esy_add ~package_name ~cache_path ~ligo_registry =
  ( ""
  , [| "esy"
     ; "add"
     ; package_name
     ; "--prefix-path"
     ; cache_path
     ; "--npm-registry"
     ; ligo_registry
    |] )


let esy_install ~cache_path ~ligo_registry =
  ( ""
  , [| "esy"
     ; "@ligo"
     ; "install"
     ; "--prefix-path"
     ; cache_path
     ; "--npm-registry"
     ; ligo_registry
    |] )


let where ~cmd = "", [| "where"; "/q"; cmd |]
let which ~cmd = "", [| "which"; cmd |]

let git_clone ~project_url ~project_name =
  "", [| "git"; "clone"; project_url; project_name |]


let git_checkout ~dir_path ~ref = "", [| "git"; "--git-dir"; dir_path; "checkout"; ref |]

let typedoc_generate ~arguments ~files =
  "", Array.concat [ [| typedoc |]; Array.of_list arguments; Array.of_list files ]
