module Constants = Cli_helpers.Constants

let run_esy package_name cache_path ligo_registry =
  let endpoint_uri = "/-/api" in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let uri_str = Uri.to_string uri in
  match Cli_helpers.does_command_exist Constants.esy with
  | Ok true ->
    let result =
      match package_name with
      | Some package_name ->
        Cli_helpers.run_command
          (Constants.esy_add ~package_name ~cache_path ~ligo_registry:uri_str)
      | None ->
        Cli_helpers.run_command (Constants.esy_install ~cache_path ~ligo_registry:uri_str)
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


let manifest_migration_prompt ~manifest =
  Printf.sprintf
    "A valid ligo.json wasn't found, but a %s was found instead.\n\
     Ligo now uses ligo.json to manage it's package dependencies.\n\
     Does it contain ligo dependencies? Would you like it to be renamed to ligo.json? "
    manifest


let migrate_manifest ~project_root ~previous_manifest =
  let ( let* ) = Caml.Result.bind in
  let previous_manifest_path = Fpath.(v project_root / previous_manifest) in
  let new_manifest_path = Fpath.(v project_root / "ligo.json") in
  let old_lock_file_path = Fpath.(v project_root / "esy.lock") in
  let* () = Bos.OS.Path.move ~force:true previous_manifest_path new_manifest_path in
  let* () = Bos.OS.Path.delete ~recurse:true old_lock_file_path in
  Ok ()


let rresult_to_ligo_result = function
  | Ok v -> Ok v
  | Error (`Msg m) -> Error (m, "")


let rec detect_project_root cwd =
  match Bos.OS.Path.exists Fpath.(cwd / "ligo.json:") with
  | Ok true -> Some cwd
  | Ok false -> if Fpath.is_root cwd then None else detect_project_root (Fpath.parent cwd)
  | Error _ ->
    (* TODO log that we failed to detect project root and that we chose to go with cwd *)
    None

let manual_migration_hint =
  "To continue, create a ligo.json with ligo dependencies in it."

let rec install
    ~project_root
    ~package_name
    ~cache_path
    ~ligo_registry
    ~package_management_alpha
  =
  let ( let* ) = Caml.Result.bind in
  let* project_root =
    match project_root with
    | Some p -> Ok p
    | None ->
      (match Bos.OS.Dir.current () with
      | Ok cwd ->
        (match detect_project_root cwd with
        | Some path -> Ok (Fpath.to_string path)
        | None -> Ok (Fpath.to_string cwd))
      | Error (`Msg m) -> Error (m, ""))
  in
  match Package_management.Alpha.does_json_manifest_exist () with
  | `Invalid_ligo_json -> Error ("Invalid manifest ligo.json", "")
  | `Invalid_esy_json -> Error ("No manifest file found", "")
  | `Valid_esy_json ->
    let msg = manifest_migration_prompt ~manifest:"esy.json" in
    let* prompt_response =
      match Lwt_main.run @@ Prompt.prompt ~msg with
      | Ok prompt_response -> Ok prompt_response
      | Error e -> Error (Prompt.error_to_string e, "")
    in
    (match prompt_response with
    | "y" | "Y" | "yes" ->
      let* () =
        rresult_to_ligo_result
        @@ migrate_manifest ~project_root ~previous_manifest:"esy.json"
      in
      install
        ~project_root:(Some project_root)
        ~package_name
        ~cache_path
        ~ligo_registry
        ~package_management_alpha
    | _ -> Error (manual_migration_hint, ""))
  | `Valid_package_json ->
    let msg = manifest_migration_prompt ~manifest:"package.json" in
    let* prompt_response =
      match Lwt_main.run @@ Prompt.prompt ~msg with
      | Ok prompt_response -> Ok prompt_response
      | Error e -> Error (Prompt.error_to_string e, "")
    in
    (match prompt_response with
    | "y" | "Y" | "yes" ->
      let* _ =
        rresult_to_ligo_result
        @@ migrate_manifest ~project_root ~previous_manifest:"package.json"
      in
      install
        ~project_root:(Some project_root)
        ~package_name
        ~cache_path
        ~ligo_registry
        ~package_management_alpha
    | _ -> Error (manual_migration_hint, ""))
  | `Invalid_package_json -> Error ("No manifest file found", "")
  | `No_manifest | `OK ->
    if not package_management_alpha
    then run_esy package_name cache_path ligo_registry
    else (
      print_endline ("Project root: " ^ project_root);
      let cache_path = Fpath.v cache_path in
      let package_name = Option.map ~f:String.strip package_name in
      Lwt_main.run
      @@ Package_management.Alpha.run ~project_root package_name cache_path ligo_registry
      |> function
      | Ok () -> Ok ("", "")
      | Error e -> Error (Package_management.Alpha.string_of_error e, ""))
