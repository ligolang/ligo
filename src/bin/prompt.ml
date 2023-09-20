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
    inherit prompt_base term
    inherit! LTerm_read_line.read_password () as _super
    initializer self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 prompt_msg))
  end

type 'a error =
  | Cancelled
  | Not_tty
  | Unknown_error of 'a

let handle_interruption f =
  let open Lwt.Syntax in
  let* stdout_term = Lazy.force LTerm.stdout in
  if LTerm.is_a_tty stdout_term
  then
    let* () = LTerm_inputrc.load () in
    Lwt.catch
      (fun () ->
        let* v = f stdout_term in
        Lwt_result.return @@ Zed_string.to_utf8 @@ v)
      (function
        | LTerm_read_line.Interrupt | Caml.Sys.Break -> Lwt_result.lift @@ Error Cancelled
        | e -> Lwt_result.lift @@ Error (Unknown_error e))
  else Lwt_result.lift @@ Error Not_tty


let prompt ~msg =
  let f stdout_term = (new prompt stdout_term msg)#run in
  handle_interruption f


let prompt_sensitive ~msg =
  let f stdout_term = (new prompt_sensitive stdout_term msg)#run in
  handle_interruption f
