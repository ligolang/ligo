open Simple_utils.Display
open Simple_utils
module Trace = Simple_utils.Trace

let format_result
    :  display_format:ex_display_format
    -> (raise:(Main_errors.all, _) Trace.raise -> 'value) -> _
  =
 fun ~display_format value ->
  let errors, warns, info =
    Trace.try_with
      ~fast_fail:false
      (fun ~raise ~catch ->
        let v = value ~raise in
        catch.errors (), catch.warnings (), Some v)
      (fun ~catch e -> e :: catch.errors (), catch.warnings (), None)
  in
  let output = Formatter.{ errors; warns; info } in
  let disp = Displayable { value = output; format = Formatter.get_scope_format } in
  let (Ex_display_format t) = display_format in
  let result_as_str : string =
    match t with
    | Human_readable -> convert ~display_format:t disp
    | Dev -> convert ~display_format:t disp
    | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
  in
  let status_is_ok = Option.is_some info in
  if status_is_ok then Ok (result_as_str, "") else Error (result_as_str, "")
