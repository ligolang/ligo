module Ligo_interface = Ligo_interface.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

open Lsp.Types

module Requests = Ligo_api.Lsp_server.Requests.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

(* TODO: Allow reading from a file. *)
type definition_test =
  { test_name : string
  ; dialect : Syntax_types.t
  ; source : string
  ; definition : Position.t
  ; reference : Position.t
  }

let get_definition_test
    ({ test_name; dialect; source; definition; reference } : definition_test)
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
  match get_scope_info with
  | _, _, Some (defs, _) ->
    (* TODO: Improve error messages. *)
    (match Requests.get_definition reference uri defs with
    | None -> Alcotest.fail "No definition found."
    | Some actual_definition ->
      let actual_loc = Scopes.Types.get_range actual_definition in
      (match Utils.position_of_location actual_loc with
      | Some actual_pos when Utils.position_equal actual_pos definition -> ()
      | _ -> Alcotest.fail "Definitions don't match."))
  | _ -> Alcotest.fail "Failed to get scopes."


let test_cases =
  [ { test_name = "Simple reference"
    ; dialect = CameLIGO
    ; source = "let x = 1\nlet y = x"
    ; definition = Position.create ~line:0 ~character:4
    ; reference = Position.create ~line:1 ~character:8
    }
  ]


let tests = "definition", List.map ~f:get_definition_test test_cases
