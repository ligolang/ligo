open Core
module Handler = Requests.Handler

(** The language server or LIGO (more commonly) might crash, leaving the language server
    in a state where this crash must be handled. This data type records what is the known
    state. *)
type t =
  | No_crash (** Operating in a normal state with no crashes. *)
  | Crashed of exn
      (** A crash has occurred. The field of this constructor indicates the recorded
          exception. *)
  | Pressed_don't_show_again_no_crash
      (** The user has pressed to not display a message indicating that a crash has
          occurred and we are operating in a normal state with no crashes. *)
  | Pressed_don't_show_again_crashed of exn
      (** The user has pressed to not display a message indicating that a crash has
          occurred and a crash has occurred. The field of this constructor indicates the
          recorded exception. *)

(** Moves from a state indicating that a crash occurred to a state indicating that no
    crash occurred. If the state indicates no crash, then this function returns the state
    unchanged. *)
let indicate_recovery : t -> t = function
  | No_crash | Crashed _ -> No_crash
  | Pressed_don't_show_again_no_crash | Pressed_don't_show_again_crashed _ ->
    Pressed_don't_show_again_no_crash


(** Moves from a state indicating that a crash occurred using the provided exception. If
    the state already indicates a crash, then the constructor will be updated with the
    provided exception. *)
let indicate_crash : exn -> t -> t =
 fun exn -> function
  | No_crash | Crashed _ -> Crashed exn
  | Pressed_don't_show_again_no_crash | Pressed_don't_show_again_crashed _ ->
    Pressed_don't_show_again_crashed exn


(** If we crash, the language client (like VSCode) might spam messages notifying the user
    that a capability has failed. To prevent this, we provide this handler which will run
    the provided handler and prevent the LSP server from dying, and will display one crash
    message to the user (if the [bool ref] is [false]), which may be disabled if they
    press "Don't show again". The [bool ref] is changed to [true] if the user has clicked
    to not display the message again. The [default] value is used in case of a crash. *)
let try_with_handler
    (type a)
    (crash_state : t ref)
    (action : a Handler.t)
    ~(default : exn -> a)
    : a Handler.t
  =
  let log_crash (exn : exn) : unit Handler.t =
    let stack = Printexc.get_backtrace () in
    Handler.send_log_msg ~type_:Error
    @@ Format.asprintf "Unexpected exception: %a\n%s" Exn.pp exn stack
  in
  let log_crash_and_display_message (exn : exn) : unit Handler.t =
    let%bind.Handler () = log_crash exn in
    let don't_show_again = "Don't show again" in
    Handler.send_message_with_buttons
      ~type_:Error
      ~options:[ don't_show_again ]
      ~message:
        (Format.asprintf
           "An unexpected exception has occurred.\n\
            The language server will now operate at a limited capacity until the crash \
            has been worked around. Please report this as a bug together with an example \
            on how to reproduce this. A stack trace may be found in the logs for LIGO \
            Language Server. Details:\n\
            %a"
           Exn.pp
           exn)
      ~handler:(function
        | Error error -> Handler.send_log_msg ~type_:Error error.message
        | Ok None -> Handler.pass
        | Ok (Some { title }) ->
          if String.equal title don't_show_again
          then crash_state := Pressed_don't_show_again_crashed exn;
          Handler.pass)
  in
  let handle_exn (exn : exn) : unit Handler.t =
    let%map.Handler () =
      match !crash_state with
      | No_crash -> log_crash_and_display_message exn
      | Crashed previous_exn ->
        Handler.when_ (not @@ Poly.equal previous_exn exn)
        @@ log_crash_and_display_message exn
      | Pressed_don't_show_again_no_crash -> log_crash exn
      | Pressed_don't_show_again_crashed previous_exn ->
        Handler.when_ (not @@ Poly.equal previous_exn exn) @@ log_crash exn
    in
    crash_state := indicate_crash exn !crash_state
  in
  Handler.with_run_in_io
  @@ fun { unlift_io } ->
  try%lwt unlift_io action with
  | exn ->
    let default_value = default exn in
    let%lwt () = unlift_io @@ handle_exn exn in
    Lwt.return default_value
