open Lsp_helpers
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler
open Range.Construct

module ModifiersSet = struct
  module Compare = struct
    type t = SemanticTokenModifiers.t

    let compare = Caml.compare
  end

  include Caml.Set.Make (Compare)

  let yojson_of_t =
    yojson_of_list SemanticTokenModifiers.yojson_of_t <@ Caml.List.of_seq <@ to_seq
end

module Token = struct
  type t =
    { line : int
    ; start_char : int
    ; length : int
    ; token_type : SemanticTokenTypes.t
    ; token_modifiers : ModifiersSet.t
    }
  [@@deriving yojson_of]

  let pp = Helpers_pretty.pp_with_yojson yojson_of_t
  let eq x y = Yojson.Safe.equal (yojson_of_t x) (yojson_of_t y)
end

type semantic_highlight_test =
  { file_name : string
  ; range : Range.t option
  }

(** Decompiles semantic token response into a list of [Token.t].
    For convenience the resulting format of line and char positions is [absolute]. *)
let decompile_tokens (encoded_tokens : int array) : Token.t list =
  let len = Array.length encoded_tokens in
  if len % 5 <> 0
  then failwithf "Tokens list length should be divisible by 5.\n Got length: %i" len ();
  let encoded_tokens = Array.to_list encoded_tokens in
  let rec aux (acc : Token.t list) (last_line : int) (last_char : int)
      : int list -> Token.t list
    = function
    | line_diff :: start_char_diff :: length :: type_ :: modifiers :: xs ->
      let line = line_diff + last_line in
      let start_char =
        if line_diff = 0 then start_char_diff + last_char else start_char_diff
      in
      let token_type = Requests.all_types.(type_) in
      let token_modifiers =
        List.init ~f:Fn.id (Array.length Requests.all_modifiers)
        |> List.fold_left ~init:ModifiersSet.empty ~f:(fun acc i ->
               if modifiers land (1 lsl i) <> 0
               then ModifiersSet.add Requests.all_modifiers.(i) acc
               else acc)
      in
      aux
        ({ line; start_char; length; token_type; token_modifiers } :: acc)
        line
        start_char
        xs
    | _ -> acc
  in
  List.rev @@ aux [] 0 0 encoded_tokens


let semantic_highlight_test ({ file_name; range } : semantic_highlight_test) : unit =
  let actual_semantic_tokens, _diagnostics =
    test_run_session
    @@ let@ file_name = open_file @@ normalize_path file_name in
       match range with
       | Some range -> Requests.on_req_semantic_tokens_range file_name range
       | None -> Requests.on_req_semantic_tokens_full file_name
  in
  match actual_semantic_tokens with
  | None -> failwith "Expected some semantic tokens list, got None"
  | Some actual_semantic_tokens ->
    let actual_semantic_tokens = decompile_tokens actual_semantic_tokens.data in
    Format.printf "%a" (Fmt.Dump.list Token.pp) actual_semantic_tokens


let%expect_test "Namespace with implements" =
  semantic_highlight_test
    { file_name = "contracts/signature/FA0.impl.jsligo"; range = Some (interval 13 0 35) };
  [%expect
    {|
    [{
       "line": 13,
       "start_char": 0,
       "length": 9,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 13,
       "start_char": 10,
       "length": 5,
       "token_type": "namespace",
       "token_modifiers": []
     };
     {
       "line": 13,
       "start_char": 16,
       "length": 10,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 13,
       "start_char": 27,
       "length": 4,
       "token_type": "namespace",
       "token_modifiers": []
     };
     {
       "line": 13,
       "start_char": 31,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 13,
       "start_char": 32,
       "length": 3,
       "token_type": "namespace",
       "token_modifiers": []
     }] |}]

let%expect_test "Module signature" =
  semantic_highlight_test
    { file_name = "contracts/signature/simple.mligo"; range = Some (range (0, 0) (4, 3)) };
  [%expect
    {|
    [{
       "line": 0,
       "start_char": 0,
       "length": 6,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 7,
       "length": 4,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 12,
       "length": 1,
       "token_type": "namespace",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 14,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 16,
       "length": 3,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 1,
       "start_char": 2,
       "length": 4,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 1,
       "start_char": 7,
       "length": 1,
       "token_type": "typeParameter",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 2,
       "length": 4,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 7,
       "length": 1,
       "token_type": "typeParameter",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 9,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 11,
       "length": 3,
       "token_type": "typeParameter",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 2,
       "length": 3,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 6,
       "length": 1,
       "token_type": "variable",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 8,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 10,
       "length": 1,
       "token_type": "typeParameter",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 12,
       "length": 2,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 3,
       "start_char": 15,
       "length": 1,
       "token_type": "typeParameter",
       "token_modifiers": []
     };
     {
       "line": 4,
       "start_char": 0,
       "length": 3,
       "token_type": "keyword",
       "token_modifiers": []
     }] |}]

let%expect_test "Whole file" =
  semantic_highlight_test
    { file_name = "contracts/single.parameter.jsligo"; range = None };
  [%expect
    {|
    [{
       "line": 0,
       "start_char": 0,
       "length": 43,
       "token_type": "macro",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 8,
       "length": 24,
       "token_type": "string",
       "token_modifiers": []
     };
     {
       "line": 0,
       "start_char": 33,
       "length": 10,
       "token_type": "string",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 0,
       "length": 5,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 6,
       "length": 17,
       "token_type": "variable",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 23,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 25,
       "length": 12,
       "token_type": "keyword",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 38,
       "length": 8,
       "token_type": "namespace",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 47,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 49,
       "length": 4,
       "token_type": "enumMember",
       "token_modifiers": []
     };
     {
       "line": 2,
       "start_char": 55,
       "length": 1,
       "token_type": "operator",
       "token_modifiers": []
     }] |}]
