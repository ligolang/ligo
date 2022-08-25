module Constants = Cli_helpers.Constants

let install ~package_name ~cache_path ~ligo_registry = 
  match Cli_helpers.does_command_exist (Constants.esy) with
    Ok true ->
      let result = 
        (match package_name with
          Some package_name -> 
            Cli_helpers.run_command (Constants.esy_add ~package_name ~cache_path ~ligo_registry)
        | None -> 
            Cli_helpers.run_command (Constants.esy_install ~cache_path ~ligo_registry)) in
      (match result with
        Ok () -> Ok ("", "")
      | Error e -> Error ("error while install packages", e)) 
  | Ok false -> Error ("No esy executable was found.\nPlease install esy (https://esy.sh/) on your system ", "")
  | Error e  -> Error (e, "") 
