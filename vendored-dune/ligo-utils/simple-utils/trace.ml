(** {1 Constructors} *)

(* Warnings *)
let warning_with f =
  let warnings = ref [] in
  let add_warning x = warnings := x :: !warnings in
  let get_warnings () = !warnings in
  f add_warning get_warnings

(* Errors *)
type ('error, 'warning) raise =
  { error : 'a. 'error -> 'a
  ; warning : 'warning -> unit
  ; log_error : 'error -> unit
  ; fast_fail : bool
  }

let raise_map_error ~f raise =
  { raise with
    error = Fn.compose raise.error f
  ; log_error = Fn.compose raise.log_error f
  }

type ('error, 'warning) catch =
  { warnings : unit -> 'warning list
  ; errors : unit -> 'error list
  }

let try_with ?(fast_fail = true) (type error warning) f g =
  let recoverable_errors = ref [] in
  let warnings = ref [] in
  let exception Local of error in
  let raise : (error, warning) raise =
    if not fast_fail
    then
      { error = (fun x -> Stdlib.raise (Local x))
      ; warning = (fun x -> warnings := x :: !warnings)
      ; log_error = (fun x -> recoverable_errors := x :: !recoverable_errors)
      ; fast_fail = false
      }
    else
      { error = (fun x -> Stdlib.raise (Local x))
      ; warning = (fun x -> warnings := x :: !warnings)
      ; log_error = (fun x -> Stdlib.raise (Local x))
      ; fast_fail = true
      }
  in
  let catch : (error, warning) catch =
    { warnings = (fun () -> List.rev !warnings)
    ; errors = (fun () -> !recoverable_errors)
    }
  in
  try f ~raise ~catch with
  | Local x -> g ~catch x

let try_with_lwt ?(fast_fail = true) (type error warning) f g =
  let recoverable_errors = ref [] in
  let warnings = ref [] in
  let exception Local_lwt of error in
  let raise : (error, warning) raise =
    let error x = Stdlib.raise (Local_lwt x) in
    let warning x = warnings := x :: !warnings in
    let log_error =
      if fast_fail then error else fun x -> recoverable_errors := x :: !recoverable_errors
    in
    { error; warning; log_error; fast_fail }
  in
  let catch : (error, warning) catch =
    { warnings = (fun () -> List.rev !warnings)
    ; errors = (fun () -> !recoverable_errors)
    }
  in
  try%lwt f ~raise ~catch with
  | Local_lwt x -> g ~catch x

type (_, _, 'error) fast_fail =
  | No_fast_fail : ('error list, 'error list, 'error) fast_fail
  | Fast_fail : ('error, unit, 'error) fast_fail

let cast_fast_fail_result = function
  | Ok (res, (), ws) -> Ok (res, [], ws)
  | Error (err, ws) -> Error ([ err ], ws)

let to_stdlib_result
    (type error warning error_error error_ok value)
    ~(fast_fail : (error_error, error_ok, error) fast_fail)
    :  (raise:(error, warning) raise -> value)
    -> (value * error_ok * warning list, error_error * warning list) Stdlib.result
  =
 fun f ->
  match fast_fail with
  | No_fast_fail ->
    try_with
      ~fast_fail:false
      (fun ~raise ~catch ->
        let ret = f ~raise in
        Ok (ret, catch.errors (), catch.warnings ()))
      (fun ~catch e -> Error (e :: catch.errors (), catch.warnings ()))
  | Fast_fail ->
    try_with
      ~fast_fail:true
      (fun ~raise ~catch ->
        let ret = f ~raise in
        Ok (ret, (), catch.warnings ()))
      (fun ~catch e -> Error (e, catch.warnings ()))

let to_stdlib_result_lwt
    (type error warning error_error error_ok value)
    ~(fast_fail : (error_error, error_ok, error) fast_fail)
    :  (raise:(error, warning) raise -> value Lwt.t)
    -> (value * error_ok * warning list, error_error * warning list) Lwt_result.t
  =
 fun f ->
  let open Lwt.Let_syntax in
  match fast_fail with
  | No_fast_fail ->
    try_with_lwt
      ~fast_fail:false
      (fun ~raise ~catch ->
        let%map v = f ~raise in
        Ok (v, catch.errors (), catch.warnings ()))
      (fun ~catch e -> Lwt.return @@ Error (e :: catch.errors (), catch.warnings ()))
  | Fast_fail ->
    try_with_lwt
      ~fast_fail:true
      (fun ~raise ~catch ->
        let%map v = f ~raise in
        Ok (v, (), catch.warnings ()))
      (fun ~catch e -> Lwt.return @@ Error (e, catch.warnings ()))

let map_error ~f ~raise t = t ~raise:(raise_map_error ~f raise)

let extract_all_errors
    : (raise:('error, _) raise -> 'value) -> 'error list * 'value option
  =
 fun f ->
  try_with
    (fun ~raise ~catch ->
      let v = f ~raise in
      catch.errors (), Some v)
    (fun ~catch e -> e :: catch.errors (), None)

let move_errors catch raise tracer =
  List.iter (catch.errors ()) ~f:(fun e -> raise.log_error (tracer e))

let move_errors_lwt catch raise tracer =
  let open Lwt.Let_syntax in
  Lwt_list.iter_s
    (fun e ->
      let%map trace' = tracer e in
      raise.log_error trace')
    (catch.errors ())

let trace_warnings ~raiser ~catcher () =
  catcher.warnings () |> List.iter ~f:raiser.warning

let trace ~raise tracer f =
  let parent_raise = raise in
  let try_body ~raise ~catch =
    let value = f ~raise in
    trace_warnings ~raiser:parent_raise ~catcher:catch ();
    move_errors catch parent_raise tracer;
    value
  in
  let catch_body ~catch err =
    trace_warnings ~raiser:parent_raise ~catcher:catch ();
    move_errors catch parent_raise tracer;
    parent_raise.error @@ tracer err
  in
  try_with ~fast_fail:parent_raise.fast_fail try_body catch_body

let trace_lwt ~raise tracer f =
  let open Lwt.Let_syntax in
  let parent_raise = raise in
  let try_body ~raise ~catch =
    let%bind value = f ~raise in
    trace_warnings ~raiser:parent_raise ~catcher:catch ();
    let%map () = move_errors_lwt catch parent_raise tracer in
    value
  in
  let catch_body ~catch err =
    trace_warnings ~raiser:parent_raise ~catcher:catch ();
    let%bind () = move_errors_lwt catch parent_raise tracer in
    Lwt.map parent_raise.error @@ tracer err
  in
  try_with_lwt ~fast_fail:parent_raise.fast_fail try_body catch_body

let trace_option ~raise error = function
  | None -> raise.error error
  | Some s -> s

let validate_option ~raise ~err ~default = function
  | None ->
    raise.log_error err;
    default
  | Some s -> s

(* Erase the current error stack, and replace it by the given
   error. It's useful when using [Assert] and you want to discard its
   autogenerated message. *)

let trace_strong ~raise err = trace ~raise (fun _ -> err)

let from_result ~raise = function
  | Ok o -> o
  | Error e -> raise.error e

(* Check if there is no error. Useful for tests. *)
let to_bool f =
  try_with
    (fun ~raise ~catch:_ ->
      let _ = f ~raise in
      true)
    (fun ~catch _ -> false)

let to_option ?fast_fail f =
  try_with ?fast_fail (fun ~raise ~catch:_ -> Some (f ~raise)) (fun ~catch _ -> None)

(* Convert an option to a result, with a given error if the parameter
   is None. *)

let trace_assert_fail_option ~raise error = function
  | None -> ()
  | Some _s -> raise.error error

let trace_assert_option ~raise error = function
  | None -> raise.error error
  | Some _s -> ()

let bind_map_or ~raise handler fa fb c =
  let handler ~raise ~catch_left ~catch_right a b =
    trace_warnings ~raiser:raise ~catcher:catch_left ();
    trace_warnings ~raiser:raise ~catcher:catch_right ();
    handler a
  in
  try_with
    (fun ~raise ~catch -> fa c ~raise)
    (fun ~catch:catch_left a ->
      try_with
        (fun ~raise ~catch -> fb c ~raise)
        (fun ~catch:catch_right b -> handler ~raise ~catch_left ~catch_right a b))

let bind_or ~raise a b = bind_map_or ~raise raise.error (fun () -> a) (fun () -> b) ()

let rec bind_exists ~raise = function
  | x, [] -> x ~raise
  | x, y :: ys -> bind_or ~raise x (bind_exists (y, ys))

let collect ~(raise : ('a list, 'w) raise) : (raise:('a, 'w) raise -> 'b) list -> 'b list =
 fun lst ->
  let errors = ref [] in
  let warns = ref [] in
  let value =
    List.map lst ~f:(fun f ->
        try_with
          (fun ~raise ~catch ->
            let v = f ~raise in
            warns := catch.warnings () :: !warns;
            Some v)
          (fun ~catch a ->
            warns := catch.warnings () :: !warns;
            errors := a :: !errors;
            None))
  in
  List.iter ~f:raise.warning @@ List.concat !warns;
  match Option.all value with
  | Some v -> v
  | None -> raise.error !errors

(* Dummy raise instance for debug and workarounds.
   Don't use it in production! *)
let raise_failwith str =
  { error = (fun _ -> failwith str)
  ; warning = (fun _ -> ())
  ; log_error = (fun _ -> failwith str)
  ; fast_fail = true
  }

(* Assertion module.
   TODO: Would make sense to move it outside Trace. *)
module Assert = struct
  let assert_fail ~raise:r err f =
    try_with
      (fun ~raise ~catch ->
        let _ = f ~raise in
        r.error err)
      (fun ~catch _ -> ())

  let assert_true ~raise err = function
    | true -> ()
    | false -> raise.error err

  let assert_list_size ~raise err lst n = assert_true ~raise err List.(length lst = n)
  let assert_list_empty ~raise err lst = assert_true ~raise err List.(length lst = 0)

  let assert_list_same_size ~raise err lsta lstb =
    assert_true ~raise err List.(length lsta = length lstb)
end
