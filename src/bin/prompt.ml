class virtual prompt_base term =
  object (self)
    inherit [Zed_string.t] LTerm_read_line.engine () as super
    inherit [Zed_string.t] LTerm_read_line.term term

    method! send_action =
      function
      | LTerm_read_line.Break -> raise Caml.Sys.Break
      | action -> super#send_action action

    method! show_box = false
  end

class prompt term (prompt_msg : string) =
  object (self)
    inherit LTerm_read_line.read_line () as _super
    inherit! prompt_base term
    initializer self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 prompt_msg))
  end

class prompt_sensitive term (prompt_msg : string) =
  object (self)
    inherit LTerm_read_line.read_password () as _super
    inherit! prompt_base term
    initializer self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 prompt_msg))
  end

let prompt_login stdout_term =
  let open Lwt.Syntax in
  let* () = LTerm_inputrc.load () in
  let username_prompt_msg = "Username: " in
  let password_prompt_message = "Password: " in
  let* user = (new prompt stdout_term username_prompt_msg)#run in
  let* pass = (new prompt_sensitive stdout_term password_prompt_message)#run in
  Lwt.return (user, pass)


let prompt_register stdout_term =
  let open Lwt.Syntax in
  let* () = LTerm_inputrc.load () in
  let username_prompt_msg = "Username: " in
  let email_prompt_msg = "Email: " in
  let password_prompt_message = "Password: " in
  let* user = (new prompt stdout_term username_prompt_msg)#run in
  let* email = (new prompt stdout_term email_prompt_msg)#run in
  let* pass = (new prompt_sensitive stdout_term password_prompt_message)#run in
  Lwt.return (user, email, pass)


let prompt_login stdout_term =
  try
    let u, p = Lwt_main.run (prompt_login stdout_term) in
    Ok (Zed_string.to_utf8 u, Zed_string.to_utf8 p)
  with
  | LTerm_read_line.Interrupt | Caml.Sys.Break -> Error ("Error: Canceled", "")


let prompt_register stdout_term =
  try
    let u, e, p = Lwt_main.run (prompt_register stdout_term) in
    Ok (Zed_string.to_utf8 u, Zed_string.to_utf8 e, Zed_string.to_utf8 p)
  with
  | LTerm_read_line.Interrupt | Caml.Sys.Break -> Error ("Error: Canceled", "")

type 'a error =
  | Cancelled
  | Unknown_error of 'a

let handle_interruption f =
  Lwt.catch f (function
      | LTerm_read_line.Interrupt | Caml.Sys.Break -> Lwt_result.lift @@ Error Cancelled
      | e -> Lwt_result.lift @@ Error (Unknown_error e))

let prompt ~msg =
  let f () =
    let open Lwt.Syntax in
    let* stdout_term = Lazy.force LTerm.stdout in
    let* () = LTerm_inputrc.load () in
    let* v = (new prompt stdout_term msg)#run in
    Lwt_result.return @@ Zed_string.to_utf8 @@ v
  in
  handle_interruption f
