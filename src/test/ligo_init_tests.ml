open Test_helpers
module OS = Bos.OS

(* new project *)
let project_mock_name = "project_folder_used_for_test"

let folder_existence_state fpath ~should_exist ~raise:_ =
  let test_project_folder_exist = OS.Dir.exists @@ fpath in
  match test_project_folder_exist with
  | Ok exist ->
    if Bool.equal exist should_exist
    then ()
    else
      failwith
      @@ "The folder "
      ^ Fpath.to_string fpath
      ^ " not compliant with should_exist : "
      ^ Bool.to_string should_exist
  | Error _msg ->
    failwith @@ "Unexpected error during testing if " ^ Fpath.to_string fpath ^ " exist"


let clone_and_check ~no_colour ~kind ~template ~registry ?project_name_opt () =
  let result =
    Ligo_init.new_project
      ~version:"mock"
      ~kind
      ~project_name_opt
      ~template
      ~display_format:Display.human_readable
      ~no_colour
      ~registry
      ()
  in
  let project_name =
    match project_name_opt with
    | Some e -> e
    | None -> template
  in
  match result with
  | Ok (_, _) ->
    let _ =
      folder_existence_state Fpath.(v "." / project_name) ~should_exist:true ~raise
    in
    let _ =
      folder_existence_state
        Fpath.(v "." / project_name / ".git")
        ~should_exist:false
        ~raise
    in
    Ok ()
  | Error (e, _) -> Error e


let test_init_new_contract_with_template ~no_colour ~registry ~raise:_ () =
  let res =
    clone_and_check
      ~no_colour
      ~kind:`CONTRACT
      ~template:"advisor-cameligo"
      ~project_name_opt:project_mock_name
      ~registry
      ()
  in
  match res with
  | Ok () -> ()
  | Error e -> failwith @@ "Unexpected error during init new project : " ^ e


let test_init_new_library_with_template ~no_colour ~registry ~raise:_ () =
  let res =
    clone_and_check
      ~no_colour
      ~kind:`LIBRARY
      ~template:"ligo-breathalyzer"
      ~project_name_opt:project_mock_name
      ~registry
      ()
  in
  match res with
  | Ok () -> ()
  | Error e -> failwith @@ "Unexpected error during init new project : " ^ e


let test_init_project_with_unexisting_template_raise_exception
    ~no_colour
    ~registry
    ~raise:_
    ()
  =
  let r =
    clone_and_check
      ~no_colour
      ~kind:`CONTRACT
      ~template:"unexisting-template"
      ~project_name_opt:project_mock_name
      ~registry
      ()
  in
  match r with
  | Ok () -> ()
  | Error err ->
    Alcotest.(check string)
      "Same error message"
      (Format.asprintf
         "Error: Unrecognized template\n\
          Hint: Use the option --template \"TEMPLATE_NAME\" \n\n\
          Please select a template from the following list: \n\
          - %s\n\
          Or check if template exists on LIGO registry.\n"
         (String.concat ~sep:"\n- " (Ligo_init.list' ~kind:`CONTRACT)))
      err


let test_init_project_with_default_name ~no_colour ~registry ~raise:_ () =
  let res =
    clone_and_check ~no_colour ~kind:`LIBRARY ~template:"ligo-breathalyzer" ~registry ()
  in
  match res with
  | Ok () -> ()
  | Error e -> failwith @@ "Unexpected error during init new project : " ^ e


let setup_test () = ()

let cleanup_test () =
  let _ = OS.Dir.delete ~recurse:true Fpath.(v "." / project_mock_name) in
  let _ = OS.Dir.delete ~recurse:true Fpath.(v "." / "ligo-breathalyzer") in
  ()


let test_new_project_wrapper ~no_colour behaviour ~raise:_ ~registry () =
  let () = setup_test () in
  let () = behaviour ~no_colour ~registry ~raise () in
  let () = cleanup_test () in
  ()


(* list project *)
let expected_contract_list =
  [ "NFT-factory-cameligo"
  ; "NFT-factory-jsligo"
  ; "advisor-cameligo"
  ; "advisor-jsligo"
  ; "dao-cameligo"
  ; "dao-jsligo"
  ; "multisig-cameligo"
  ; "multisig-jsligo"
  ; "permit-cameligo"
  ; "permit-jsligo"
  ; "predictive-market-cameligo"
  ; "predictive-market-jsligo"
  ; "randomness-cameligo"
  ; "randomness-jsligo"
  ; "shifumi-cameligo"
  ; "shifumi-jsligo"
  ]


let expected_library_list =
  [ "ligo-bigarray"
  ; "ligo-breathalyzer"
  ; "ligo-extendable-fa2"
  ; "ligo-fa"
  ; "ligo-math-lib"
  ; "ligo-permit"
  ]


let test_init_list_template_contract_template_with_format ~raise:_ () =
  let result_list = Ligo_init.list' ~kind:`CONTRACT in
  assert (List.equal String.equal expected_contract_list result_list)


let test_init_list_template_library_template_with_format ~raise:_ () =
  let result_list = Ligo_init.list' ~kind:`LIBRARY in
  assert (List.equal String.equal expected_library_list result_list)


let test_init_list_template_contract_template ~no_colour ~raise:_ () =
  let expected_output =
    "list of projects:\n" ^ String.concat ~sep:"\n" expected_contract_list ^ "\n"
  in
  let result_list =
    Ligo_init.list ~kind:`CONTRACT ~display_format:Display.human_readable ~no_colour ()
  in
  let _ =
    match result_list with
    | Ok (result, _w) ->
      Alcotest.(check string) "same output format" expected_output result
    | Error _ -> failwith @@ "Error during list result evaluation"
  in
  ()


let main =
  let no_colour = Test_helpers.options.tools.no_colour in
  let registry = Commands.Constants.ligo_registry |> Uri.of_string in
  let test_new_project_wrapper = test_new_project_wrapper ~no_colour ~registry in
  let test_init_list_template_contract_template =
    test_init_list_template_contract_template ~no_colour
  in
  test_suite
    "LIGO init tests"
    [ test
        "list template contract with format"
        test_init_list_template_contract_template_with_format
    ; test
        "list template library with format"
        test_init_list_template_library_template_with_format
    ; test "list template project" test_init_list_template_contract_template
    ; test
        "init new contract with template"
        (test_new_project_wrapper test_init_new_contract_with_template)
    ; test
        "init new library with template"
        (test_new_project_wrapper test_init_new_library_with_template)
    ; test
        "init new project with unexisting template"
        (test_new_project_wrapper
           test_init_project_with_unexisting_template_raise_exception)
    ; test
        "init new project with project name defaulted by template name"
        (test_new_project_wrapper test_init_project_with_default_name)
    ]
