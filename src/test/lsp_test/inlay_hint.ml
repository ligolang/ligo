module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

let get_inlay_hint_test file_path : unit =
  let hints_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path file_path in
       Requests.on_req_inlay_hint uri Range.whole_file
  in
  match hints_opt with
  | Some actual_hints -> Format.printf "%a" (Fmt.Dump.list InlayHint.pp) actual_hints
  | None -> failwith "Expected some list of inlay hints, got None"


let%expect_test "Inlay hints in CameLIGO" =
  get_inlay_hint_test "contracts/lsp/inlay_hints/inlay_hints.mligo";
  [%expect
    {|
    [{ "label": "(", "position": { "character": 7, "line": 6 } };
     { "kind": 1, "label": ": int", "position": { "character": 8, "line": 6 } };
     { "label": ")", "position": { "character": 8, "line": 6 } };
     { "label": "(", "position": { "character": 15, "line": 6 } };
     { "kind": 1, "label": ": int", "position": { "character": 16, "line": 6 } };
     { "label": ")", "position": { "character": 16, "line": 6 } };
     { "kind": 1, "label": ": int", "position": { "character": 9, "line": 8 } };
     { "kind": 1, "label": ": int", "position": { "character": 19, "line": 8 } };
     { "kind": 1, "label": ": a", "position": { "character": 9, "line": 10 } };
     { "kind": 1, "label": ": b", "position": { "character": 12, "line": 10 } };
     { "kind": 1, "label": ": a", "position": { "character": 15, "line": 12 } };
     { "kind": 1, "label": ": a", "position": { "character": 18, "line": 12 } };
     { "label": "(", "position": { "character": 20, "line": 12 } };
     {
       "kind": 1,
       "label": ": bool",
       "position": { "character": 21, "line": 12 }
     }; { "label": ")", "position": { "character": 21, "line": 12 } };
     { "label": "(", "position": { "character": 30, "line": 16 } };
     { "kind": 1, "label": ": int", "position": { "character": 31, "line": 16 } };
     { "label": ")", "position": { "character": 31, "line": 16 } };
     { "label": "(", "position": { "character": 8, "line": 26 } };
     { "kind": 1, "label": ": int", "position": { "character": 9, "line": 26 } };
     { "label": ")", "position": { "character": 9, "line": 26 } };
     { "label": "(", "position": { "character": 10, "line": 26 } };
     { "kind": 1, "label": ": int", "position": { "character": 11, "line": 26 } };
     { "label": ")", "position": { "character": 11, "line": 26 } };
     { "label": "(", "position": { "character": 31, "line": 28 } };
     { "kind": 1, "label": ": int", "position": { "character": 32, "line": 28 } };
     { "label": ")", "position": { "character": 32, "line": 28 } };
     { "kind": 1, "label": ": int", "position": { "character": 10, "line": 29 } };
     { "kind": 1, "label": ": int", "position": { "character": 13, "line": 29 } };
     { "kind": 1, "label": ": int", "position": { "character": 16, "line": 30 } };
     { "kind": 1, "label": ": int", "position": { "character": 19, "line": 30 } };
     { "kind": 1, "label": ": a", "position": { "character": 14, "line": 35 } };
     { "label": "(", "position": { "character": 8, "line": 37 } };
     { "kind": 1, "label": ": a t", "position": { "character": 11, "line": 37 } };
     { "label": ")", "position": { "character": 11, "line": 37 } };
     { "kind": 1, "label": ": a", "position": { "character": 9, "line": 39 } };
     { "kind": 1, "label": ": int", "position": { "character": 7, "line": 42 } };
     { "kind": 1, "label": ": int", "position": { "character": 9, "line": 54 } };
     { "kind": 1, "label": ": int", "position": { "character": 12, "line": 54 } };
     {
       "kind": 1,
       "label": ": string",
       "position": { "character": 15, "line": 54 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 14, "line": 55 } };
     { "kind": 1, "label": ": int", "position": { "character": 22, "line": 55 } };
     {
       "kind": 1,
       "label": ": string",
       "position": { "character": 30, "line": 55 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 10, "line": 58 } };
     { "kind": 1, "label": ": b", "position": { "character": 13, "line": 58 } };
     { "kind": 1, "label": ": c", "position": { "character": 16, "line": 58 } };
     { "kind": 1, "label": ": int", "position": { "character": 6, "line": 0 } };
     {
       "kind": 1,
       "label": ": 'a.int",
       "position": { "character": 15, "line": 4 }
     };
     {
       "kind": 1,
       "label": ": int -> int",
       "position": { "character": 8, "line": 6 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 16, "line": 6 } };
     {
       "kind": 1,
       "label": ": int -> int",
       "position": { "character": 10, "line": 8 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 20, "line": 8 } };
     { "kind": 1, "label": ": a", "position": { "character": 13, "line": 10 } };
     {
       "kind": 1,
       "label": ": 'a.('a * 'a) -> bool -> 'a",
       "position": { "character": 6, "line": 12 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 21, "line": 12 } };
     { "kind": 1, "label": ": int", "position": { "character": 31, "line": 16 } };
     { "kind": 1, "label": ": unit", "position": { "character": 7, "line": 18 } };
     { "kind": 1, "label": ": unit", "position": { "character": 7, "line": 25 } };
     { "kind": 1, "label": ": int", "position": { "character": 11, "line": 26 } };
     { "kind": 1, "label": ": int", "position": { "character": 32, "line": 28 } };
     { "kind": 1, "label": ": int", "position": { "character": 14, "line": 29 } };
     {
       "kind": 1,
       "label": ": (int * int) -> int",
       "position": { "character": 7, "line": 30 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 20, "line": 30 } };
     { "kind": 1, "label": ": a", "position": { "character": 15, "line": 35 } };
     { "kind": 1, "label": ": a", "position": { "character": 11, "line": 37 } };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 24, "line": 41 }
     };
     { "kind": 1, "label": ": unit", "position": { "character": 7, "line": 45 } };
     {
       "kind": 1,
       "label": ": bool",
       "position": { "character": 13, "line": 46 }
     };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 19, "line": 53 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 17, "line": 58 } }] |}]

let%expect_test "Inlay hints in JsLIGO" =
  get_inlay_hint_test "contracts/lsp/inlay_hints/inlay_hints.jsligo";
  [%expect
    {|
    [{ "kind": 1, "label": ": int", "position": { "character": 8, "line": 0 } };
     {
       "kind": 1,
       "label": ": <a>(_: unit) => int",
       "position": { "character": 8, "line": 4 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 13, "line": 6 } };
     { "label": "(", "position": { "character": 10, "line": 7 } };
     { "kind": 1, "label": ": int", "position": { "character": 11, "line": 7 } };
     { "label": ")", "position": { "character": 11, "line": 7 } };
     { "kind": 1, "label": ": int", "position": { "character": 13, "line": 10 } };
     { "kind": 1, "label": ": int", "position": { "character": 12, "line": 11 } };
     { "kind": 1, "label": ": a", "position": { "character": 15, "line": 14 } };
     { "kind": 1, "label": ": b", "position": { "character": 18, "line": 14 } };
     {
       "kind": 1,
       "label": ": <a>(_: [a, a], z: bool) => a",
       "position": { "character": 8, "line": 18 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 14, "line": 18 } };
     { "kind": 1, "label": ": a", "position": { "character": 17, "line": 18 } };
     {
       "kind": 1,
       "label": ": bool",
       "position": { "character": 21, "line": 18 }
     };
     {
       "kind": 1,
       "label": ": (x: int) => int",
       "position": { "character": 8, "line": 34 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 9, "line": 44 } };
     {
       "kind": 1,
       "label": ": bool",
       "position": { "character": 12, "line": 45 }
     };
     {
       "kind": 1,
       "label": ": (x: int, y: int) => int",
       "position": { "character": 9, "line": 52 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 14, "line": 52 } };
     { "kind": 1, "label": ": int", "position": { "character": 17, "line": 52 } };
     {
       "kind": 1,
       "label": ": (x: int, y: int) => int",
       "position": { "character": 10, "line": 53 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 24, "line": 53 } };
     { "kind": 1, "label": ": int", "position": { "character": 27, "line": 53 } };
     {
       "kind": 1,
       "label": ": (_: unit) => int",
       "position": { "character": 10, "line": 54 }
     };
     {
       "kind": 1,
       "label": ": (x: int) => int",
       "position": { "character": 9, "line": 55 }
     };
     {
       "kind": 1,
       "label": ": (_: [int, int]) => int",
       "position": { "character": 9, "line": 57 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 15, "line": 57 } };
     { "kind": 1, "label": ": int", "position": { "character": 18, "line": 57 } };
     {
       "kind": 1,
       "label": ": (_: [int, int]) => int",
       "position": { "character": 9, "line": 58 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 24, "line": 58 } };
     { "kind": 1, "label": ": int", "position": { "character": 27, "line": 58 } };
     {
       "kind": 1,
       "label": ": t<a>",
       "position": { "character": 17, "line": 63 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 14, "line": 65 } };
     { "kind": 1, "label": ": int", "position": { "character": 12, "line": 70 } };
     { "kind": 1, "label": ": bool", "position": { "character": 9, "line": 76 } };
     { "kind": 1, "label": ": int", "position": { "character": 9, "line": 85 } };
     { "kind": 1, "label": ": int", "position": { "character": 12, "line": 85 } };
     {
       "kind": 1,
       "label": ": string",
       "position": { "character": 15, "line": 85 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 14, "line": 86 } };
     { "kind": 1, "label": ": int", "position": { "character": 22, "line": 86 } };
     {
       "kind": 1,
       "label": ": string",
       "position": { "character": 30, "line": 86 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 16, "line": 89 } };
     { "kind": 1, "label": ": b", "position": { "character": 19, "line": 89 } };
     { "kind": 1, "label": ": c", "position": { "character": 22, "line": 89 } };
     { "kind": 1, "label": ": int", "position": { "character": 16, "line": 4 } };
     {
       "kind": 1,
       "label": ": (y: int) => int",
       "position": { "character": 14, "line": 6 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 11, "line": 7 } };
     {
       "kind": 1,
       "label": ": (y: int) => int",
       "position": { "character": 14, "line": 10 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 13, "line": 11 } };
     { "kind": 1, "label": ": a", "position": { "character": 20, "line": 14 } };
     { "kind": 1, "label": ": a", "position": { "character": 22, "line": 18 } };
     { "kind": 1, "label": ": int", "position": { "character": 15, "line": 43 } };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 14, "line": 51 }
     };
     { "kind": 1, "label": ": int", "position": { "character": 18, "line": 52 } };
     { "kind": 1, "label": ": int", "position": { "character": 28, "line": 53 } };
     { "kind": 1, "label": ": int", "position": { "character": 24, "line": 54 } };
     { "kind": 1, "label": ": int", "position": { "character": 20, "line": 57 } };
     { "kind": 1, "label": ": int", "position": { "character": 29, "line": 58 } };
     { "kind": 1, "label": ": a", "position": { "character": 18, "line": 63 } };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 29, "line": 69 }
     };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 14, "line": 75 }
     };
     {
       "kind": 1,
       "label": ": unit",
       "position": { "character": 24, "line": 84 }
     };
     { "kind": 1, "label": ": a", "position": { "character": 24, "line": 89 } }] |}]

let%expect_test "Inlay hints for type recovered type variables" =
  get_inlay_hint_test "contracts/lsp/inlay_hints/typer_error_recovery.jsligo";
  [%expect
    {|
    [{
       "kind": 1,
       "label": ": [^a, ^b]",
       "position": { "character": 7, "line": 0 }
     };
     { "kind": 1, "label": ": ^c", "position": { "character": 7, "line": 1 } };
     { "kind": 1, "label": ": ^d", "position": { "character": 7, "line": 2 } };
     {
       "kind": 1,
       "label": ": [^c, ^d, [^a, ^b]]",
       "position": { "character": 7, "line": 3 }
     }] |}]
