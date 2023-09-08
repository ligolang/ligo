module Constants = Cli_helpers.Constants

let run_esy package_name cache_path ligo_registry =
  match Cli_helpers.does_command_exist Constants.esy with
  | Ok true ->
    let result =
      match package_name with
      | Some package_name ->
        Cli_helpers.run_command
          (Constants.esy_add ~package_name ~cache_path ~ligo_registry)
      | None -> Cli_helpers.run_command (Constants.esy_install ~cache_path ~ligo_registry)
    in
    (match result with
    | Ok () -> Ok ("", "")
    | Error e -> Error ("error while install packages", e))
  | Ok false ->
    Error
      ( "No esy executable was found.\n\
         Please install esy (https://esy.sh/) on your system "
      , "" )
  | Error e -> Error (e, "")


let install
    ~project_root
    ~package_name
    ~cache_path
    ~ligo_registry
    ~package_management_alpha
  =
  match Package_management.Alpha.does_json_manifest_exist () with
  | Error e -> Error (e, "")
  | Ok () ->
    let run_esy () = run_esy package_name cache_path ligo_registry in
    if not package_management_alpha
    then run_esy ()
    else (
      let package_name = Option.value ~default:"" package_name in
      let package_name = String.strip package_name in
      let cache_path = Fpath.v cache_path in
      let ligo_registry = Uri.of_string ligo_registry in
      Lwt_main.run
      @@ Package_management.Alpha.run ~project_root package_name cache_path ligo_registry
      |> function
      | Ok () -> Ok ("", "")
      | Error e -> Error (Package_management.Alpha.string_of_error e, ""))
