module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_helpers
open Requests.Handler

type formatting_test =
  { expected_path : string
  ; output_path_opt : string option
  }

let get_formatting_test ({ expected_path; output_path_opt } : formatting_test) : unit =
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ Path.from_relative expected_path in
       Requests.on_req_formatting uri FormattingOptions.default
  in
  match result, output_path_opt with
  | None, None -> ()
  | None, Some _ -> failwith "Could not format."
  | Some _, None -> failwith "Can format, but expected to fail and return None."
  | Some [ { range; newText } ], Some output_path ->
    if not (Range.equal Range.whole_file range)
    then failwith "Expected a whole_file_range.";
    Out_channel.write_all output_path ~data:newText
  | Some _, _ -> failwith "Formatting returned multiple edits."


let () =
  let expected_path, output_path_opt =
    match Sys.get_argv () with
    | [| _; expected; output |] -> expected, Some output
    | [| _; expected |] -> expected, None
    | _ -> failwith "Expected 1 or 2 arguments"
  in
  get_formatting_test { expected_path; output_path_opt }
