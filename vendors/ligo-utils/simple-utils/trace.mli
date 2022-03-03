(*
Warning with is a wrapper for warning.
It creates a function to add warning to a list and get the list of warning.
Theses function is then pass the function given.
Generally the wrapper will be use as such:
warning_with @@ fun add_warning get_warnings ->
  let result = function_that_emit_warnings ~add_warning in
  let warnings = get_warnings () in
  rest_of_the processing ()
*)
val warning_with : (('a -> unit) -> (unit -> 'a list) -> 'b) -> 'b


(* [try_with] is a wrapper for error that may works in two modes:
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

type 't raise = { raise : 'a . 't -> 'a;
                 log_error : 't -> unit;
                 get_errors : unit -> 't list;
                 fast_fail : bool; }
(* [try_with f handler] call [f] with [~raise] argument and in case of fatal
   error [err] call [handler err]. *)
val try_with  : ?fast_fail:bool -> (raise:'t raise -> 'b) -> ('t -> 'b) -> 'b
(* Similar to [try_with] but allows use [~raise] in the handler *)
val try_with' : ?fast_fail:bool -> (raise:'t raise -> 'b) -> (raise: 't raise -> 't -> 'b) -> 'b
(*
Wrap the [try_with] in a stdlib [result = Ok 'value | Error 'error]
 *)
val to_stdlib_result : (raise:'error raise -> 'value) -> ('value, 'error) result

(* Wrap [try_wait'] and return value and all logged errors with fatal one if it happens *)
val extract_all_errors : (raise:'error raise -> 'value) -> 'error list * 'value option

(*
Act as a map for the propagated error. Save [fast_fail] mode.
*)
val trace : raise:'b raise -> ('a -> 'b) -> (raise:'a raise -> 'c) -> 'c
(* Similar but erase the previous error instead of casting it *)
val trace_strong : raise:'a raise -> 'a -> (raise:'b raise -> 'c) -> 'c

(* Unwrap an option using our own error instead of exception *)
val trace_option : raise:'a raise -> 'a -> 'b option -> 'b
(* Check that option contains some value otherwise returns default and log error *)
val validate_option : raise:'a raise -> err:'a -> default:'b -> 'b option -> 'b

(* Raise error if the option is Some *)
val trace_assert_fail_option : raise:'a raise -> 'a -> 'b option -> unit

(* Raise error if the option is None *)
val trace_assert_option : raise:'a raise -> 'a -> 'b option -> unit

(* Unwrap the result, raising the error if needed *)
val from_result  : raise:'b raise -> ('a,'b) result -> 'a

(* Check if the function is not failing *)
val to_bool : (raise:'b raise -> 'a) -> bool

(* Return the evaluation of the functino as Some(res) | None *)
val to_option : (raise:'b raise -> 'a) -> 'a option

(* Run the second function if the first fails *)
val bind_or : raise:'a raise -> (raise:'b raise -> 'c) -> (raise:'a raise -> 'c) -> 'c
val bind_exists : raise:'a raise -> ((raise:'a raise -> 'b) * (raise:'a raise -> 'b) list) -> 'b
val bind_map_or :
  ('a -> 'b) -> ('c -> raise:'d raise -> 'b) -> ('c -> raise:'a raise -> 'b) ->
  'c -> 'b

(* Dummy raise instance for debug and workarounds.
   Don't use it in production! *)
val raise_failwith : string -> 't raise

(*
Assert module, raise exception if the assertion is false
*)
module Assert :
sig
  val assert_fail : raise:'a raise -> 'a -> (raise:'b raise -> 'c) -> unit
  val assert_true : raise:'a raise -> 'a -> bool -> unit
  val assert_list_size : raise:'a raise -> 'a -> 'b list -> int -> unit
  val assert_list_empty : raise:'a raise -> 'a -> 'b list -> unit
  val assert_list_same_size : raise:'a raise -> 'a -> 'b list -> 'c list -> unit
end
