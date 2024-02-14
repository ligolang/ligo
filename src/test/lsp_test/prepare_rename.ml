module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

type prepare_rename_test =
  { file_path : Path.t (* To support packaged files *)
  ; reference : Position.t
  ; can_rename : bool
  }

let get_prepare_rename_test ({ file_path; reference; can_rename } : prepare_rename_test)
    : unit
  =
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file file_path in
       Requests.on_req_prepare_rename reference uri
  in
  let error_prefix =
    Format.asprintf "In %s, %a: " (Path.to_string file_path) Position.pp reference
  in
  match result with
  | None ->
    if can_rename
    then failwith @@ error_prefix ^ "Cannot prepare rename for this reference."
    else ()
  | Some actual_range ->
    if can_rename
    then
      if Range.contains_position reference actual_range
      then ()
      else failwith @@ error_prefix ^ "Reference is not contained within the range."
    else
      failwith
      @@ error_prefix
      ^ "Should not be able to rename this identifier, but we can."


let%test_unit "Identifier (definition)" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:4
    ; can_rename = true
    }

let%test_unit "Identifier (reference)" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; can_rename = true
    }

let%test_unit "Identifier from stdlib" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:5 ~character:8
    ; can_rename = false
    }

let%test_unit "Number" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:8
    ; can_rename = false
    }

let%test_unit "Type" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:2 ~character:8
    ; can_rename = true
    }

let%test_unit "Module" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:16
    ; can_rename = true
    }

let%test_unit "Built-in type" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:1 ~character:11
    ; can_rename = false
    }

let%test_unit "Type from stdlib" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:28
    ; can_rename = false
    }

let%test_unit "Keyword" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:0
    ; can_rename = false
    }

let%test_unit "Registry package imported identifier" =
  get_prepare_rename_test
    { file_path = normalize_path "contracts/lsp/registry.jsligo"
    ; reference = Position.create ~line:9 ~character:20
    ; can_rename = false
    }

let%test_unit "Registry package identifier" =
  get_prepare_rename_test
    { file_path =
        Path.from_absolute
        @@ Option.value_exn
        @@ Lsp_test_helpers.Lib.resolve_lib_path
             ~project_root:(resolve "contracts/lsp")
             ~file:(resolve "contracts/lsp/registry.jsligo")
             ~lib_name:"bigarray"
             ~file_path:(Filename.concat "lib" "bigarray.mligo")
    ; reference = Position.create ~line:27 ~character:4
    ; can_rename = false
    }
