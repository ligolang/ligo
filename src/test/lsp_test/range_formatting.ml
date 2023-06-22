module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Range.Construct
open Requests.Handler

type range_formatting_test =
  { file_path : string
  ; range : Range.t
  ; expected : (string * Range.t) option
  }

let get_formatting_test ({ file_path; range; expected } : range_formatting_test) : unit =
  (* Not creating an Alcotest.test_case here because we want to test many cases
     without printing too many lines in log.*)
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative file_path) in
       Requests.on_req_range_formatting uri range FormattingOptions.default
  in
  let mk_message s =
    Format.asprintf "Range formatting: %s. %s, %a" s file_path Range.pp range
  in
  match result, expected with
  | None, None -> ()
  | None, Some _ -> fail @@ mk_message "Expected some TextEdit, got none"
  | Some _, None -> fail @@ mk_message "Expected to return None, but returned some edits"
  | Some [ { range; newText } ], Some (expected_string, expected_range) ->
    check
      Alcotest.string
      (mk_message "Replacement contents does not match the expected")
      expected_string
      newText;
    check
      Range.testable
      (mk_message "Replacement range does not match the expected")
      expected_range
      range
  | Some _, _ -> fail "Returned multiple edits, which was not expected"


let test_cases_cameligo =
  (* Creating many test cases with similar ranges (so same declarations are selected)
  without too much boilerplate *)
  let tests_no_statements =
    List.map
      [ range (1, 0) (2, 0); interval 4 16 39; interval 6 16 39 ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.mligo"; range = r; expected = None })
  in
  let tests_one_directive =
    List.map
      [ range (0, 0) (1, 0); interval 0 0 34 ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.mligo"
        ; range = r
        ; expected = Some ("#include \"simple.mligo\"", interval 0 0 34)
        })
    @ List.map
        [ range (1, 0) (3, 0); interval 2 0 39; range (1, 0) (4, 12) ]
        ~f:(fun r ->
          { file_path = "contracts/lsp/format_me.mligo"
          ; range = r
          ; expected = Some ("#import \"local_module.mligo\" \"A\"", interval 2 0 39)
          })
  in
  let tests_one_declaration =
    List.map
      [ range (4, 0) (5, 0); interval 4 0 39 ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.mligo"
        ; range = r
        ; expected = Some ("let format_me =\n  let x = 20 in\n  x * 2", interval 4 0 39)
        })
    @ List.map
        [ range (5, 0) (6, 0); interval 5 0 40 ]
        ~f:(fun r ->
          { file_path = "contracts/lsp/format_me.mligo"
          ; range = r
          ; expected =
              Some ("let format_me_2 =\n  let q = A.A.s in\n  q ^ q", interval 5 0 40)
          })
  in
  let tests_two_declarations =
    List.map
      [ range (4, 0) (6, 0); range (4, 0) (6, 0); range (4, 0) (7, 0) ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.mligo"
        ; range = r
        ; expected =
            Some
              ( "let format_me =\n\
                \  let x = 20 in\n\
                \  x * 2\n\n\
                 let format_me_2 =\n\
                \  let q = A.A.s in\n\
                \  q ^ q"
              , range (4, 0) (5, 40) )
        })
  in
  let test_whole_file =
    [ { file_path = "contracts/lsp/format_me.mligo"
      ; range = Range.whole_file
      ; expected =
          Some
            ( String.strip (In_channel.read_all "contracts/lsp/formatted.mligo")
            , range (0, 0) (5, 40) )
      }
    ]
  in
  tests_no_statements
  @ tests_one_directive
  @ tests_one_declaration
  @ tests_two_declarations
  @ test_whole_file


let test_cases_jsligo =
  let tests_no_statements =
    List.map
      [ range (1, 0) (2, 0); interval 8 0 1; interval 11 0 10 ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.jsligo"; range = r; expected = None })
  in
  let tests_one_declaration =
    List.map
      [ range (1, 0) (5, 0); range (2, 0) (3, 62) ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.jsligo"
        ; range = r
        ; expected =
            Some
              ( "const increment = (b: int): int => ((a: int): int => a + 1)(b);"
              , range (2, 0) (3, 62) )
        })
    @ List.map
        [ range (4, 0) (6, 0); interval 5 0 52 ]
        ~f:(fun r ->
          { file_path = "contracts/lsp/format_me.jsligo"
          ; range = r
          ; expected =
              Some ("const with_semicolon = [1 + 2 * 3, M.fold_test];", interval 5 0 52)
          })
    @ List.map
        [ range (6, 0) (13, 0); range (7, 0) (12, 27) ]
        ~f:(fun r ->
          { file_path = "contracts/lsp/format_me.jsligo"
          ; range = r
          ; expected =
              Some
                ( "const incr_map = (l: list<int>): list<int> => List.map((i: int) => i \
                   + 1, l);"
                , range (7, 0) (12, 27) )
          })
  in
  let tests_two_declarations =
    List.map
      [ range (1, 0) (6, 0); range (2, 0) (5, 52) ]
      ~f:(fun r ->
        { file_path = "contracts/lsp/format_me.jsligo"
        ; range = r
        ; expected =
            Some
              ( "const increment = (b: int): int => ((a: int): int => a + 1)(b);\n\n\
                 const with_semicolon = [1 + 2 * 3, M.fold_test];"
              , range (2, 0) (5, 52) )
        })
    @ List.map
        [ range (5, 0) (13, 0); range (5, 0) (12, 27) ]
        ~f:(fun r ->
          { file_path = "contracts/lsp/format_me.jsligo"
          ; range = r
          ; expected =
              Some
                ( "const with_semicolon = [1 + 2 * 3, M.fold_test];\n\n\
                   const incr_map = (l: list<int>): list<int> => List.map((i: int) => i \
                   + 1, l);"
                , range (5, 0) (12, 27) )
          })
  in
  let test_whole_file =
    [ { file_path = "contracts/lsp/format_me.jsligo"
      ; range = Range.whole_file
      ; expected =
          Some
            ( String.strip (In_channel.read_all "contracts/lsp/formatted.jsligo")
            , range (0, 0) (12, 27) )
      }
    ]
  in
  tests_no_statements @ tests_one_declaration @ tests_two_declarations @ test_whole_file


let tests : string * unit Alcotest.test_case list =
  ( "range_formatting"
  , [ (Alcotest.test_case "cameligo" `Quick
      @@ fun () -> List.iter ~f:get_formatting_test test_cases_cameligo)
    ; (Alcotest.test_case "jsligo" `Quick
      @@ fun () -> List.iter ~f:get_formatting_test test_cases_jsligo)
    ] )
