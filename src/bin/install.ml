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


class migration_prompt term (prompt_msg : string) =
  object (self)
    inherit LTerm_read_line.read_line () as super
    inherit [Zed_string.t] LTerm_read_line.term term

    method! send_action =
      function
      | LTerm_read_line.Break -> raise Caml.Sys.Break
      | action -> super#send_action action

    method! show_box = false
    initializer self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 prompt_msg))
  end

let prompt_esy_json_migration stdout_term =
  let open Lwt.Syntax in
  let* () = LTerm_inputrc.load () in
  let prompt_msg =
    "A valid ligo.json wasn't found, but a package.json was found instead. Ligo\n\
    \ now uses ligo.json to manage it's package dependencies. Does it contain\n\
    \ ligo dependencies? Would you like it to be renamed to ligo.json? "
  in
  let prompt = new migration_prompt stdout_term prompt_msg in
  let* migrate_esy_json = prompt#run in
  Lwt.return migrate_esy_json


let prompt_esy_json_migration stdout_term =
  try
    Result.return
    @@ Zed_string.to_utf8
    @@ Lwt_main.run
    @@ prompt_esy_json_migration stdout_term
  with
  | LTerm_read_line.Interrupt | Caml.Sys.Break -> Error ("Canceled!", "")


let prompt_package_json_migration stdout_term =
  let open Lwt.Syntax in
  let* () = LTerm_inputrc.load () in
  let prompt_msg =
    "A valid ligo.json wasn't found, but a package.json was found instead. Ligo\n\
    \ now uses ligo.json to manage it's package dependencies. Does it contain\n\
    \ ligo dependencies? Would you like it to be renamed to ligo.json? "
  in
  let* migrate_package_json = (new migration_prompt stdout_term prompt_msg)#run in
  Lwt.return migrate_package_json


let prompt_package_json_migration stdout_term =
  try
    Result.return
    @@ Zed_string.to_utf8
    @@ Lwt_main.run
    @@ prompt_package_json_migration stdout_term
  with
  | LTerm_read_line.Interrupt | Caml.Sys.Break -> Error ("Canceled!", "")


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


let rec install
    ~project_root
    ~package_name
    ~cache_path
    ~ligo_registry
    ~package_management_alpha
  =
  let ( let* ) = Caml.Result.bind in
  let project_root =
    match project_root with
    | Some p -> p
    | None -> Caml.Sys.getcwd ()
  in
  match Package_management.Alpha.does_json_manifest_exist () with
  | `Invalid_ligo_json -> Error ("Invalid manifest ligo.json", "")
  | `Invalid_esy_json -> Error ("No manifest file found", "")
  | `Valid_esy_json ->
    let stdout_term = Lwt_main.run @@ Lazy.force LTerm.stdout in
    let prompt_response =
      if LTerm.is_a_tty stdout_term
      then prompt_esy_json_migration stdout_term
      else Result.return @@ Caml.Sys.getenv "LIGO_MIGRATE_ESY_JSON"
    in
    (match prompt_response with
    | Ok ("y" | "Y" | "yes") ->
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
    | _ -> Error ("Valid manifest not found and old manifest w\nsn't migrated", ""))
  | `Valid_package_json ->
    let stdout_term = Lwt_main.run @@ Lazy.force LTerm.stdout in
    let prompt_response =
      if LTerm.is_a_tty stdout_term
      then prompt_package_json_migration stdout_term
      else Result.return @@ Caml.Sys.getenv "LIGO_MIGRATE_PACKAGE_JSON"
    in
    (match prompt_response with
    | Ok ("y" | "Y" | "yes") ->
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
    | _ -> Error ("Valid manifest not found and old manifest w\nsn't migrated", ""))
  | `Invalid_package_json -> Error ("No manifest file found", "")
  | `No_manifest -> Error ("No manifest file found", "")
  | `OK ->
    if not package_management_alpha
    then run_esy package_name cache_path ligo_registry
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
