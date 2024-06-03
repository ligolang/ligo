open Core
module Display = Simple_utils.Display
module Trace = Simple_utils.Trace

(** Helper function to format the result of [ligo info get-scope]. Formats scopes,
    definitions, errors, and warnings. *)
let format_result
    :  display_format:Display.ex_display_format -> no_colour:bool
    -> (raise:(Main_errors.all, _) Trace.raise -> 'value) -> _
  =
 fun ~display_format ~no_colour value ->
  let errors, warns, info =
    Trace.try_with
      ~fast_fail:false
      (fun ~raise ~catch ->
        let v = value ~raise in
        catch.errors (), catch.warnings (), Some v)
      (fun ~catch e -> e :: catch.errors (), catch.warnings (), None)
  in
  let output : Scopes_format.get_scope_output = Scopes_format.{ errors; warns; info } in
  let disp =
    Display.Displayable { value = output; format = Scopes_format.get_scope_format }
  in
  let (Ex_display_format t) = display_format in
  let result_as_str : string =
    match t with
    | Human_readable -> Display.convert ~display_format:t ~no_colour disp
    | Dev -> Display.convert ~display_format:t ~no_colour disp
    | Json ->
      Yojson.Safe.pretty_to_string @@ Display.convert ~display_format:t ~no_colour disp
  in
  let status_is_ok = Option.is_some info in
  if status_is_ok then Ok (result_as_str, "") else Error (result_as_str, "")
