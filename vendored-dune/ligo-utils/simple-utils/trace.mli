(** [try_with] is a wrapper for error that may works in two modes:
   - [fast_fail = false] i.e. recovery mode that allows raise a fatal error by
     [raise exn] or remember non-fatal errors by [log_error exn] that can be
     retrieved by [get_errors ()];
   - [fast_fail = true] in that [log_error] behaves like [raise].

   The main goal is to defer decisions whether to raise or log an error and how to
   handle the former case. It allows combine and reuse recoverable pipelines that
   try to accumulate as most as can errors and non-recoverable pipelines.

   Generally the wrapper will be use in fast fail mode as such :
     [let result = try_with f handler]
   where f is a function that will use the raise function to throw error
   and handler is called with the error parameter is the exception was raised

   In the non-"fast fail" mode it will used as such:
   [let sum_option_int (v1 v2 : int option) : 'e list * int =
      try_with' ~fast_fail:true
        (fun ~raise ->
             let v1 = validate_option ~raise ~err:InvalidArgument ~default:0 v1 in
             let v2 = validate_option ~raise ~err:InvalidArgument ~default:0 v2 in
             (raise.get_errors (), v1 + v2)
        (fun ~raise err -> (err :: raise.get_errors (), 0)
   ] *)

type ('error, 'warning) raise =
  { error : 'a. 'error -> 'a
  ; warning : 'warning -> unit
  ; log_error : 'error -> unit
  ; fast_fail : bool
  }

val raise_map_error
  :  f:('error1 -> 'error2)
  -> ('error2, 'warning) raise
  -> ('error1, 'warning) raise

type ('error, 'warning) catch =
  { warnings : unit -> 'warning list
  ; errors : unit -> 'error list
  }

(** [try_with f handler] call [f] with [~raise] argument and in case of fatal
    error [err] call [handler err]. *)
val try_with
  :  ?fast_fail:bool
  -> (raise:('error, 'warning) raise -> catch:('error, 'warning) catch -> 'a)
  -> (catch:('error, 'warning) catch -> 'error -> 'a)
  -> 'a

(** [try_with_lwt f handler] works similarly to [try_with], but it uses [Lwt.catch] under
    the hood, allowing Lwt's rejected promises to be handled. Very important: whenever
    your handlers return Lwt promises, you must use this function instead of [try_with]. *)
val try_with_lwt
  :  ?fast_fail:bool
  -> (raise:('error, 'warning) raise -> catch:('error, 'warning) catch -> 'a Lwt.t)
  -> (catch:('error, 'warning) catch -> 'error -> 'a Lwt.t)
  -> 'a Lwt.t

(** Whether to recover from [raise.log_error] ([No_fast_fast]) or immediately fail
    [Fast_fail]. The three fields represent, respectively, the type of the error(s) on
    [Result.Error], the type of the optional error on [Result.Ok], and the type of the
    error that is used by the other two fields. *)
type (_, _, 'error) fast_fail =
  | No_fast_fail : ('error list, 'error list, 'error) fast_fail
      (** Do not fail on [log_error]. The three fields represent, respectively, the type
          of error raised on [Result.Error], the type of error raised on [Result.Ok], and
          the type of the error. *)
  | Fast_fail : ('error, unit, 'error) fast_fail
      (** Fail on [log_error]. The three fields represent, respectively, the type of error
          raised on [Result.Error], the type of error raised on [Result.Ok] ([unit] since
          we fail on the first error), and the type of the error. *)

(** Wrap the [try_with] in a stdlib [result = Ok 'value | Error 'error]. *)
val to_stdlib_result
  :  fast_fail:('error_error, 'error_ok, 'error) fast_fail
  -> (raise:('error, 'warning) raise -> 'value)
  -> ('value * 'error_ok * 'warning list, 'error_error * 'warning list) Stdlib.result

(** Wrap the [try_with_lwt] in a [Lwt_result.t]. Very important: whenever your handler
    return a Lwt promise, you must use this function instead of [to_stdlib_result]. *)
val to_stdlib_result_lwt
  :  fast_fail:('error_error, 'error_ok, 'error) fast_fail
  -> (raise:('error, 'warning) raise -> 'value Lwt.t)
  -> ('value * 'error_ok * 'warning list, 'error_error * 'warning list) Lwt_result.t

val map_error
  :  f:('error1 -> 'error2)
  -> raise:('error2, 'warn) raise
  -> (raise:('error1, 'warn) raise -> 'value)
  -> 'value

(** Wrap [try_wait'] and return value and all logged errors with fatal one if it happens *)
val extract_all_errors
  :  (raise:('error, _) raise -> 'value)
  -> 'error list * 'value option

(** Act as a map for the propagated error. Save [fast_fail] mode. *)
val trace : raise:('b, 'w) raise -> ('a -> 'b) -> (raise:('a, 'w) raise -> 'c) -> 'c

(** Similar to [trace] but erase the previous error instead of casting it. *)
val trace_strong : raise:('a, 'w) raise -> 'a -> (raise:('b, 'w) raise -> 'c) -> 'c

(** Similar to [trace] but with the same considerations as [try_with_lwt]. Very important:
    use this function whenever your handler returns a Lwt promise. *)
val trace_lwt
  :  raise:('b, 'w) raise
  -> ('a -> 'b Lwt.t)
  -> (raise:('a, 'w) raise -> 'c Lwt.t)
  -> 'c Lwt.t

(** collect multples errors into a list *)
val collect : raise:('a list, 'w) raise -> (raise:('a, 'w) raise -> 'b) list -> 'b list

(** Unwrap an option using our own error instead of exception *)
val trace_option : raise:('a, 'w) raise -> 'a -> 'b option -> 'b

(** Check that option contains some value otherwise returns default and log error *)
val validate_option : raise:('a, 'w) raise -> err:'a -> default:'b -> 'b option -> 'b

(** Raise error if the option is [Some]. *)
val trace_assert_fail_option : raise:('a, 'w) raise -> 'a -> 'b option -> unit

(** Raise error if the option is [None]. *)
val trace_assert_option : raise:('a, 'w) raise -> 'a -> 'b option -> unit

(** Unwrap the result, raising the error if needed. *)
val from_result : raise:('b, 'w) raise -> ('a, 'b) result -> 'a

(** Check if the function is not failing. *)
val to_bool : (raise:('b, 'w) raise -> 'a) -> bool

(** Return the evaluation of the function as [Some(res) | None]. *)
val to_option : (raise:('b, 'w) raise -> 'a) -> 'a option

(** Run the second function if the first fails. *)
val bind_or
  :  raise:('a, 'w) raise
  -> (raise:('a, 'w) raise -> 'c)
  -> (raise:('b, 'w) raise -> 'c)
  -> 'c

val bind_exists
  :  raise:('a, 'w) raise
  -> (raise:('a, 'w) raise -> 'b) * (raise:('a, 'w) raise -> 'b) list
  -> 'b

val bind_map_or
  :  raise:('e, 'w) raise
  -> ('a -> 'b)
  -> ('c -> raise:('a, 'w) raise -> 'b)
  -> ('c -> raise:('d, 'w) raise -> 'b)
  -> 'c
  -> 'b

(** Dummy raise instance for debug and workarounds.
   Don't use it in production! *)
val raise_failwith : string -> ('e, 'w) raise

(**
Assert module, raise exception if the assertion is false
*)
module Assert : sig
  val assert_fail : raise:('a, 'w) raise -> 'a -> (raise:('b, 'w) raise -> 'c) -> unit
  val assert_true : raise:('a, 'w) raise -> 'a -> bool -> unit
  val assert_list_size : raise:('a, 'w) raise -> 'a -> 'b list -> int -> unit
  val assert_list_empty : raise:('a, 'w) raise -> 'a -> 'b list -> unit
  val assert_list_same_size : raise:('a, 'w) raise -> 'a -> 'b list -> 'c list -> unit
end
