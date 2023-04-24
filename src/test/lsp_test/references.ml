module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type references_test =
  { test_name : string
  ; test_file : string
  ; reference : Position.t
  ; references : (string * Range.t) list
  }

let get_references_test
    ({ test_name; test_file; reference; references } : references_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let actual_references, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (to_absolute test_file) in
       Requests.on_req_references reference uri
  in
  let expected_references =
    List.map
      ~f:(fun (file, range) ->
        Location.create ~uri:(DocumentUri.of_path @@ to_absolute file) ~range)
      references
  in
  match actual_references with
  | None ->
    Alcotest.fail
    @@ Format.asprintf
         "Expected to find Some references for %s, but found None."
         test_file
  | Some actual_references ->
    should_match_list
      Location.testable
      ~msg:
        (Format.asprintf
           "References mismatch for: %s, %a"
           test_file
           Position.pp
           reference)
      ~expected:expected_references
      ~actual:actual_references


let test_cases =
  [ { test_name = "references in included file"
    ; test_file = "contracts/lsp/includer/includer.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; references =
        [ "contracts/lsp/includer/includer.mligo", Range.Construct.interval 1 8 9
        ; "contracts/lsp/included.mligo", Range.Construct.interval 0 4 5
        ]
    }
  ]


let tests = "references", List.map ~f:get_references_test test_cases
