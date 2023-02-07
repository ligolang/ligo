module Ligo_interface_tools = Ligo_interface

module Ligo_interface = Ligo_interface.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

open Lsp.Types
module Requests = Ligo_api.Lsp_server.Requests

(* TODO: Allow reading from a file. *)
type prepare_rename_test =
  { test_name : string
  ; dialect : Syntax_types.t
  ; source : string
  ; reference : Position.t
  ; can_rename : bool
  }

let get_prepare_rename_test
    ({ test_name; dialect; source; reference; can_rename } : prepare_rename_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let file =
    match dialect with
    | CameLIGO -> "test.mligo"
    | JsLIGO -> "test.jsligo"
  in
  let uri = DocumentUri.of_path file in
  let get_scope_info = Ligo_interface.get_scope uri source in
  let get_scope_info = Ligo_interface_tools.unfold_get_scope get_scope_info in
  (* TODO: Improve error messages. *)
  match Requests.prepare_rename reference uri get_scope_info.definitions with
  | None ->
    if can_rename then Alcotest.fail "Cannot prepare rename for this reference." else ()
  | Some actual_range ->
    if can_rename
    then
      if Utils.is_position_in_range reference actual_range
      then ()
      else Alcotest.fail "Reference is not contained within the range."
    else Alcotest.fail "Should not be able to rename this identifier, but we can."


let test_cases =
  [ { test_name = "Identifier"
    ; dialect = CameLIGO
    ; source = "let x = 1\nlet y = x"
    ; reference = Position.create ~line:1 ~character:8
    ; can_rename = true
    }
  ; { test_name = "Type"
    ; dialect = CameLIGO
    ; source = "type number = int\nlet x : number = 0"
    ; reference = Position.create ~line:1 ~character:8
    ; can_rename = true
    }
  ; { test_name = "Module"
    ; dialect = JsLIGO
    ; source = "namespace A\n{const x = 0\n}\nconst x = A.x"
    ; reference = Position.create ~line:3 ~character:10
    ; can_rename = true
    }
  ; { test_name = "Built-in type"
    ; dialect = JsLIGO
    ; source = "const str : string = \"\""
    ; reference = Position.create ~line:0 ~character:13
    ; can_rename = false
    }
  ; { test_name = "Keyword"
    ; dialect = CameLIGO
    ; source = "let x = 1"
    ; reference = Position.create ~line:0 ~character:0
    ; can_rename = false
    }
  ]


let tests = "prepare_rename", List.map ~f:get_prepare_rename_test test_cases
