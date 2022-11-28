module Constants = Cli_helpers.Constants

let does_json_manifest_exist () =
  let cwd = Sys_unix.getcwd () in
  let package_json = Filename.concat cwd "package.json" in
  match Sys_unix.file_exists package_json with
  | `Yes ->
    (try
       let _ = Yojson.Safe.from_file package_json in
       Ok ()
     with
    | _ -> Error "Invalid package.json")
  | `No | `Unknown -> Error "A package.json does not exist"


let install ~package_name ~cache_path ~ligo_registry =
  match does_json_manifest_exist () with
  | Error e -> Error (e, "")
  | Ok () ->
    (match Cli_helpers.does_command_exist Constants.esy with
    | Ok true ->
      let result =
        match package_name with
        | Some package_name ->
          Cli_helpers.run_command
            (Constants.esy_add ~package_name ~cache_path ~ligo_registry)
        | None ->
          Cli_helpers.run_command (Constants.esy_install ~cache_path ~ligo_registry)
      in
      (match result with
      | Ok () -> Ok ("", "")
      | Error e -> Error ("error while install packages", e))
    | Ok false ->
      Error
        ( "No esy executable was found.\n\
           Please install esy (https://esy.sh/) on your system "
        , "" )
    | Error e -> Error (e, ""))
