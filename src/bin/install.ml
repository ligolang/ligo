module Command = struct
  let esy = "esy"
  let esy_add = fun package_name -> ("", [|"esy"; "add"; package_name|])
  let esy_install = ("", [|"esy"; "install"|])
end

let install package_name = 
  match Cli_helpers.does_command_exist (Command.esy) with
    Ok true ->
      let result = 
        (match package_name with
          Some package_name -> 
            Cli_helpers.run_command (Command.esy_add package_name)
        | None -> 
            Cli_helpers.run_command Command.esy_install) in
      (match result with
        Ok () -> Ok ("", "")
      | Error e -> Error ("error while install packages", e)) 
  | Ok false -> Error ("No esy executable was found.\nPlease install esy (https://esy.sh/) on your system ", "")
  | Error e  -> Error (e, "") 
