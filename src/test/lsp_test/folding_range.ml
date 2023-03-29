open Linol_lwt
open Handlers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type folding_range_test =
  { file_path : string
  ; folding_ranges : FoldingRange.t list
  }

let eq_folding_range : FoldingRange.t -> FoldingRange.t -> bool = Caml.( = )

let pp_folding_range : FoldingRange.t Fmt.t =
 fun formatter fold -> Yojson.Safe.pretty_print formatter @@ FoldingRange.yojson_of_t fold


let testable_folding_range : FoldingRange.t Alcotest.testable =
  Alcotest.testable pp_folding_range eq_folding_range


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
    Common.should_match_list
      testable_folding_range
      ~actual:actual_folds
      ~expected:folding_ranges
  | None -> Alcotest.fail "Expected some list of folding ranges, got None"


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
        ; mk_region (16, 24) (19, 2)
        ]
    }
  ; { file_path = "contracts/lsp/import.jsligo"
    ; folding_ranges = [ mk_import (0, 1) (3, 14) ]
    }
  ]


let tests = "folding range", List.map ~f:get_folding_range_test test_cases
