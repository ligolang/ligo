module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type folding_range_test =
  { file_path : string
  ; folding_ranges : FoldingRange.t list
  ; contains_in_check : bool (* TODO: remove this once folding ranges is fixed *)
  }

let get_folding_range_test
    ({ file_path; folding_ranges; contains_in_check } : folding_range_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let folds_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative file_path) in
       Requests.on_req_folding_range uri
  in
  match folds_opt with
  | Some actual_folds ->
    if contains_in_check
    then
      should_be_contained_in FoldingRange.testable ~small:folding_ranges ~big:actual_folds
    else
      should_match_list
        FoldingRange.testable
        ~actual:actual_folds
        ~expected:folding_ranges
  | None -> fail "Expected some list of folding ranges, got None"


let mk_folding_range kind (startLine, startCharacter) (endLine, endCharacter) =
  FoldingRange.create ~startLine ~startCharacter ~endLine ~endCharacter ~kind ()


let mk_region = mk_folding_range FoldingRangeKind.Region
let mk_import = mk_folding_range FoldingRangeKind.Imports

let test_cases =
  [ { file_path = "contracts/lsp/folding_range.mligo"
    ; contains_in_check = false
    ; folding_ranges =
        [ mk_region (0, 1) (2, 12)
        ; mk_region (1, 5) (2, 12)
        ; mk_region (4, 1) (7, 20)
        ; mk_region (5, 3) (7, 20)
        ; mk_region (6, 5) (7, 20)
        ; mk_region (6, 5) (7, 20)
        ; mk_region (9, 1) (12, 4)
        ; mk_region (10, 3) (12, 4)
        ; mk_region (14, 1) (17, 4)
        ; mk_region (15, 3) (17, 4)
        ]
    }
  ; { file_path = "contracts/lsp/folding_range.jsligo"
    ; contains_in_check = false
    ; folding_ranges =
        [ mk_region (0, 1) (2, 24)
        ; mk_region (1, 3) (2, 24)
        ; mk_region (4, 1) (9, 2)
        ; mk_region (4, 7) (9, 2)
        ; mk_region (4, 19) (9, 2)
        ; mk_region (4, 42) (9, 2)
        ; mk_region (5, 3) (8, 4)
        ; mk_region (5, 23) (8, 4)
        ; mk_region (11, 1) (14, 2)
        ; mk_region (11, 15) (14, 2)
        ; mk_region (16, 1) (19, 2)
        ; mk_region (16, 7) (19, 2)
        ; mk_region (16, 24) (19, 2) (* TODO: delete on folding range fix *)
        ]
    }
  ; { file_path = "contracts/lsp/import.jsligo"
    ; contains_in_check = false
    ; folding_ranges =
        [ mk_import (0, 1) (3, 14) (* TODO: delete on folding range fix *)
        ; mk_region (0, 8) (3, 2)
        ]
    }
  ; { file_path = "contracts/lsp/folding_range_for_loop.jsligo"
    ; contains_in_check = true
    ; folding_ranges =
        [ mk_region (0, 21) (19, 2)
        ; mk_region (1, 36) (3, 6)
        ; mk_region (4, 5) (8, 6)
        ; mk_region (6, 17) (8, 6)
        ; mk_region (10, 5) (12, 16)
        ; mk_region (13, 36) (18, 6)
        ; mk_region (14, 40) (17, 10)
        ]
    }
  ]


let tests = "folding_range", List.map ~f:get_folding_range_test test_cases
