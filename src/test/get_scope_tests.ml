module Display = Simple_utils.Display
module Scopes = Ligo_main.Main.Scopes.Api_helper
module InfoApi = Ligo_api.Info
module Raw_options = Compiler_options.Raw_options

let schema = "../main/scopes/schema.json"

let validate_json_file file_name =
  let command_str = Format.sprintf "python3 -m jsonschema -i %s %s" file_name schema in
  Format.printf "command: %s\n" command_str;
  let status = Sys_unix.command @@ command_str in
  if status > 0 then Alcotest.fail "JSON schema validation failed"


let schema_test_positive
    ?(no_colour = false)
    ?(with_types = false)
    ?(defs_only = false)
    ?(speed = `Quick)
    source_file
  =
  let _test () =
    let temp_file_name = Filename_unix.temp_file ~in_dir:"./" "get_scope_test" ".json" in
    let write data = Out_channel.write_all temp_file_name ~data in
    let options = Raw_options.make ~with_types () in
    match
      Lsp_helpers.Ligo_interface.Get_scope.get_scope_cli_result
        options
        ~source_file
        ~display_format:Display.json
        ~no_colour
        ~defs_only
    with
    | Ok (res_str, _) ->
      write res_str;
      validate_json_file temp_file_name
    | Error (_, _) -> Alcotest.fail "Expected no errors in positive test"
  in
  Alcotest.test_case (Filename.basename source_file) speed _test


let schema_test_negative
    ?(no_colour = false)
    ?(with_types = false)
    ?(defs_only = false)
    ?(speed = `Quick)
    ?(expected_status = Some true)
    ?error_cnt
    source_file
  =
  let _test () =
    let temp_file_name = Filename_unix.temp_file ~in_dir:"./" "get_scope_test" ".json" in
    let write data = Out_channel.write_all temp_file_name ~data in
    let options = Raw_options.make ~with_types () in
    let res_str, actual_status =
      match
        Lsp_helpers.Ligo_interface.Get_scope.get_scope_cli_result
          options
          ~source_file
          ~display_format:Display.json
          ~no_colour
          ~defs_only
      with
      | Ok (res_str, _) ->
        res_str, true (* Alcotest.fail "None errors are detected in negative test" *)
      | Error (res_str, _) -> res_str, false
    in
    match expected_status with
    | Some expected_status ->
      Alcotest.(check bool) "Unexpected status" expected_status actual_status
    | None ->
      ();
      write res_str;
      validate_json_file temp_file_name;
      let open Yojson.Safe in
      let json = from_string res_str in
      let errors = json |> Util.member "errors" in
      Yojson.Safe.pretty_print Format.std_formatter errors;
      let errors = errors |> Util.to_list in
      (match error_cnt with
      | None ->
        if List.is_empty errors then Alcotest.fail "Expect non empty list of errors"
      | Some error_cnt ->
        Alcotest.(check int) "Count of errors" error_cnt (List.length errors))
  in
  Alcotest.test_case (Filename.basename source_file) speed _test


let files_in_dir dir_path =
  Sys_unix.readdir dir_path
  |> Array.to_list
  |> List.filter ~f:(fun x ->
         match Filename.split_extension x with
         | _, Some "mligo" | _, Some "jsligo" | _, Some "ligo" -> true
         | _ -> false)
  |> List.map ~f:(fun x -> dir_path ^ "/" ^ x)


let files_in_all_dirs ?(except = []) dirs =
  List.join (List.map dirs ~f:files_in_dir)
  |> List.filter ~f:(fun x -> not @@ List.mem except x ~equal:String.equal)


let _main =
  Printexc.record_backtrace true;
  Alcotest.run
    "get-scope json validation tests"
    [ ( "positive"
      , [ schema_test_positive ~with_types:false "contracts/address.mligo"
        ; schema_test_positive ~with_types:true "contracts/address.mligo"
        ; schema_test_positive ~with_types:false "contracts/incr_decr.mligo"
        ; schema_test_positive ~with_types:true "contracts/incr_decr.mligo"
        ; schema_test_positive ~with_types:false "contracts/FA1.2.mligo"
        ; schema_test_positive ~with_types:true "contracts/FA1.2.mligo"
        ] )
    ; ( "negative"
      , [ schema_test_negative
            ~with_types:false
            "error-recovery/simple/cameligo/missing_expr_parenthesesL.mligo"
        ; schema_test_negative
            ~with_types:false
            ~expected_status:(Some false)
            "error-recovery/simple/cameligo/unreadable_symbol.mligo"
        ; schema_test_negative
            ~with_types:false
            ~error_cnt:2
            "error-recovery/multiple_errors/2_errors.mligo"
        ; schema_test_negative
            ~with_types:false
            ~error_cnt:3
            "error-recovery/multiple_errors/3_errors.mligo"
        ] )
    ; ( "all_positive"
      , List.map
          (files_in_all_dirs
             [ "error-recovery/simple/cameligo/original"
             ; "error-recovery/simple/jsligo/original"
             ; "contracts"
             ]
             ~except:
               [ "contracts/tuples_no_annotation.jsligo"
               ; (* syntax error: wrong brackets + untyped recursion *)
                 "contracts/parametric_types.jsligo"
               ; (* TODO: syntax error in case *)
                 "contracts/existential.mligo"
               ])
          ~f:(fun file -> schema_test_positive ~with_types:true ~speed:`Slow file) )
    ; ( "all_negative"
      , List.map
          (files_in_all_dirs
             [ "error-recovery/simple/cameligo"; "error-recovery/simple/jsligo" ]
             ~except:
               [ "error-recovery/simple/jsligo/missing_semicolon_in_top_level.jsligo"
               ; (* was fixed by changes to jsligo ASI recently *)
                 "error-recovery/simple/jsligo/missing_type_annotation_in_lambda_in_match.jsligo"
                 (* was fixed by recent change to jsligo parser *)
               ])
          ~f:(fun file ->
            schema_test_negative ~with_types:true ~speed:`Slow file ~expected_status:None)
      )
    ]
