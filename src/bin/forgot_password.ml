module LigoRC = Cli_helpers.LigoRC

let get_auth_token ~ligorc_path ligo_registry =
  let ligorc = LigoRC.read ~ligorc_path in
  let registry_key = LigoRC.registry_key ligo_registry in
  let token = LigoRC.get_token ~registry_key ligorc in
  match token with
  | Some token -> Ok token
  | None -> Error ()


let no_session_found_msg =
  "We need a valid session with you, for security reasons,\n\
   to initiate resetting your password. We couldn't find such a session. Either,\n\
   to initiate resetting your password. We couldn't find such a session.\n\
   You could:\n\
   1. try this from a machine where you had logged in earlier,\n\
   2. contact LIGO support to reset your password"


let main ~ligo_registry ~ligorc_path ~username =
  let open Registry.Profile in
  let open Registry.Forgot_password in
  match main ~ligo_registry ~ligorc_path ~username with
  | Ok Password_reset_emailed ->
    Ok ("An email containing the link to reset your password has been sent", "")
  | Ok Profile_creation_pending ->
    let ( let* ) = Caml.Result.bind in
    let prompt () =
      let open Lwt_result.Syntax in
      let* email = Prompt.prompt ~msg:"Email: " in
      let* fullname = Prompt.prompt ~msg:"Full name (optional): " in
      Lwt_result.return (email, fullname)
    in
    let whoami ~token =
      match Registry.Whoami.main ~token ~ligo_registry with
      | Ok (Some username) -> Ok (username, "")
      | Ok None -> Error (no_session_found_msg, "")
      | Error msg -> Error (msg, "")
    in
    let setup_profile ~token ~email ~fullname =
      match Lwt_main.run @@ create ~token ~ligo_registry ~email ~fullname with
      | Ok () ->
        Ok
          ( "Your profile has been created. To verify your address, we have sent you an\n\
             email. Please follow the instructions there. You may run this command again\n\
             after verifying your address."
          , "" )
      | Error Invalid_input -> Error ("Invalid input", "")
      | Error Invalid_token ->
        Error
          ( Printf.sprintf "A session was found, but it didn't belong to user: %s" username
          , "" )
      | Error Server_error ->
        Error ("Registry is having issues right now. Please try again later", "")
    in
    let* token, _ =
      match get_auth_token ~ligorc_path ligo_registry with
      | Error () -> Error (no_session_found_msg, "")
      | Ok token -> Ok (token, "")
    in
    let* session_username, _ = whoami ~token in
    (match String.equal session_username username with
    | true ->
      print_endline
        "We don't have your email address and other details. Setting up your profile";
      (match Lwt_main.run @@ prompt () with
      | Ok (email, fullname) -> setup_profile ~token ~email ~fullname
      | Error (Prompt.Unknown_error e) -> Error (Exn.to_string e, "")
      | Error Prompt.Cancelled -> Error ("Aborted", "")
      | Error Prompt.Not_tty -> Error ("Not an interactive terminal", ""))
    | false -> Error (no_session_found_msg, ""))
  | Ok Email_verification_pending ->
    Ok
      ( "We have your email, but, it hasn't been verified.\n\
         To be safe, we just sent you a verification email.\n\
         Could you please follow the instructions there, before we send you the password \
         reset link?"
      , "" )
  | Error msg -> Error (msg, "")
