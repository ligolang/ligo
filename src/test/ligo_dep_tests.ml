open Test_helpers

let test_ligo_dep_cameligo file_name ~raise () =
  let result =
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_types.Syntax_name "auto") @@ Some file_name
    in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let meta = Ligo_compile.Of_source.extract_meta syntax in
    let c_unit, _ =
      Ligo_compile.Of_source.preprocess_file
        ~raise
        ~options:options.frontend
        ~meta
        file_name
    in
    let stdlib = Build.Stdlib.get ~options in
    let core = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let deps =
      Build.Ligo_dep_cameligo.dependencies ~std_lib:stdlib.content_typed.pr_module core
    in
    let deps = List.map ~f:Simple_utils.Location.unwrap deps in
    let deps = List.map deps ~f:(String.concat ~sep:"/") in
    List.iter deps ~f:(fun dep -> eprintf "%s\n%!" dep);
    let deps = Set.of_list (module String) deps in
    let expected =
      Set.of_list
        (module String)
        [ "E1"
        ; "E2"
        ; "E3"
        ; "E4"
        ; "E5"
        ; "E6"
        ; "E7"
        ; "E8"
        ; "E9"
        ; "E10"
        ; "E11"
        ; "Super__/E13"
        ; "Directory_2/Directory_3/E13"
        ; "Directory/E12"
        ]
    in
    if Set.equal deps expected
    then ()
    else Core.raise @@ Failure "Unexpected dependencies"
  in
  Alcotest.(check unit) "Ligo_dep_cameligo" result ()


let test_ligo_dep_jsligo file_name ~raise () =
  let result =
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_types.Syntax_name "auto") @@ Some file_name
    in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let meta = Ligo_compile.Of_source.extract_meta syntax in
    let c_unit, _ =
      Ligo_compile.Of_source.preprocess_file
        ~raise
        ~options:options.frontend
        ~meta
        file_name
    in
    let core = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let deps = Build.Ligo_dep_jsligo.dependencies core in
    let deps = List.map ~f:Simple_utils.Location.unwrap deps in
    Fmt.pr "%a\n" (Fmt.Dump.list Fmt.string) deps;
    let deps = Set.of_list (module String) deps in
    let expected =
      Set.of_list
        (module String)
        [ "./Test1"
        ; "./Test2"
        ; "./Test3"
        ; "./Test4"
        ; "./Test5"
        ; "./Test6"
        ; "./Test7"
        ; "./Test8"
        ]
    in
    if Set.equal deps expected
    then ()
    else Core.raise @@ Failure "Unexpected dependencies"
  in
  Alcotest.(check unit) "Ligo_dep_jsligo" result ()


let with_file file_name case = test file_name (case file_name)

let main =
  test_suite
    "Ligo dep tests"
    [ with_file "./contracts/import_decls.mligo" test_ligo_dep_cameligo
    ; with_file "./contracts/import_decls.jsligo" test_ligo_dep_jsligo
    ]
