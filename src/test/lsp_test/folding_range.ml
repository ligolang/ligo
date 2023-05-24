module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type folding_range_test =
  { file_path : string
  ; folding_ranges : FoldingRange.t list
  }

let get_folding_range_test ({ file_path; folding_ranges } : folding_range_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let folds_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (to_absolute file_path) in
       Requests.on_req_folding_range uri
  in
  match folds_opt with
  | Some actual_folds ->
    should_match_list FoldingRange.testable ~actual:actual_folds ~expected:folding_ranges
  | None -> fail "Expected some list of folding ranges, got None"


let mk_folding_range kind (startLine, startCharacter) (endLine, endCharacter) =
  let startCharacter = Some startCharacter in
  let endCharacter = Some endCharacter in
  FoldingRange.create ~startLine ?startCharacter ~endLine ?endCharacter ~kind ()


let mk_region = mk_folding_range FoldingRangeKind.Region
let mk_import = mk_folding_range FoldingRangeKind.Imports

let test_cases =
  [ { file_path = "contracts/lsp/folding_range.mligo"
    ; folding_ranges =
        [ mk_region (0, 1) (2, 12)
        ; mk_region (1, 5) (2, 12)
        ; mk_region (1, 5) (1, 12)
        ; mk_region (2, 5) (2, 12)
        ; mk_region (4, 1) (7, 4)
        ; mk_region (4, 15) (4, 28)
        ; mk_region (5, 3) (7, 4)
        ; mk_region (6, 5) (6, 21)
        ; mk_region (6, 5) (7, 20)
        ; mk_region (9, 1) (12, 4)
        ; mk_region (10, 3) (12, 4)
        ; mk_region (14, 1) (17, 4)
        ; mk_region (15, 3) (17, 4)
        ; mk_region (16, 5) (16, 16)
        ; mk_region (15, 5) (15, 15) (* TODO: delete on folding range fix *)
        ; mk_region (4, 16) (4, 27)
        ; mk_region (6, 5) (6, 12)
        ; mk_region (6, 5) (7, 20)
        ; mk_region (7, 5) (7, 12)
        ; mk_region (10, 5) (10, 17)
        ; mk_region (11, 5) (11, 18)
        ; mk_region (16, 14) (16, 16)
        ]
    }
  ; { file_path = "contracts/lsp/folding_range.jsligo"
    ; folding_ranges =
        [ mk_region (0, 1) (2, 24)
        ; mk_region (1, 5) (1, 24)
        ; mk_region (2, 5) (2, 24)
        ; mk_region (4, 1) (9, 2)
        ; mk_region (4, 7) (9, 2)
        ; mk_region (4, 19) (9, 2)
        ; mk_region (4, 19) (4, 31)
        ; mk_region (4, 20) (4, 30)
        ; mk_region (4, 42) (9, 2)
        ; mk_region (5, 3) (8, 4)
        ; mk_region (6, 21) (6, 33)
        ; mk_region (7, 21) (7, 32)
        ; mk_region (11, 1) (14, 2)
        ; mk_region (11, 15) (14, 2)
        ; mk_region (16, 1) (19, 2)
        ; mk_region (16, 7) (19, 2)
        ; mk_region (16, 24) (19, 2) (* TODO: delete on folding range fix *)
        ; mk_region (1, 7) (1, 22)
        ; mk_region (2, 7) (2, 22)
        ; mk_region (4, 20) (4, 30)
        ; mk_region (4, 7) (4, 16)
        ; mk_region (5, 11) (5, 21)
        ; mk_region (5, 16) (5, 21)
        ; mk_region (12, 3) (12, 15)
        ; mk_region (13, 3) (13, 16)
        ; mk_region (16, 7) (16, 12)
        ; mk_region (17, 3) (17, 12)
        ; mk_region (18, 3) (18, 15)
        ]
    }
  ; { file_path = "contracts/lsp/import.jsligo"
    ; folding_ranges =
        [ mk_import (0, 1) (3, 14) (* TODO: delete on folding range fix *)
        ; mk_region (0, 8) (3, 2)
        ]
    }
  ]


let tests = "folding_range", List.map ~f:get_folding_range_test test_cases
