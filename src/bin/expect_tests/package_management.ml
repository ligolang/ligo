open Cli_expect

let () = Caml.Sys.chdir "../../test/projects/"
let pwd = Caml.Sys.getcwd ()

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "originate_contract/test.mligo"
    ; "--project-root"
    ; "originate_contract"
    ; "--no-warn"
    ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value KT1QVWJTnMi6XJFPpnASjbfi53qokforNwdP(None). |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "measure-contract"
    ; "using_scope_pkg_project/src/a/b/c/contract.mligo"
    ; "--project-root"
    ; "using_scope_pkg_project"
    ];
  [%expect {|
    95 bytes |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; "originate_contract/main.mligo"
    ; "--project-root"
    ; "originate_contract"
    ];
  [%expect
    {|
    File "originate_contract/main.mligo", line 1, characters 0-30:
      1 | #import "tezos-ligo-fa2" "FA2"
      2 |
    File "tezos-ligo-fa2" not found. |}]

let () = Caml.Sys.chdir "using_scope_pkg_project"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "src/a/b/c/contract.test.mligo"; "--project-root"; "." ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "src/a/b/c/contract.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a/b/c"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "contract.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a/b"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "c/contract.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "b/c/contract.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "a/b/c/contract.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "using_scope_pkg_project/src/a/b/c/contract.test.mligo"
    ; "--project-root"
    ; "using_scope_pkg_project"
    ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; "dao_path_bug/main.mligo"; "--project-root"; "dao_path_bug" ];
  [%expect
    {|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]

let () = Caml.Sys.chdir "dao_path_bug"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "include_include/main.mligo"
    ; "--project-root"
    ; "include_include"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "Hello" ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "include_include"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "Hello" ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "include_import/main.mligo"
    ; "--project-root"
    ; "include_import"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "include_import"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "import_import/main.mligo"
    ; "--project-root"
    ; "import_import"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "import_import"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "import_include/main.mligo"
    ; "--project-root"
    ; "import_include"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "import_include"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "using_ligo_breathalyser/test.mligo"
    ; "--project-root"
    ; "using_ligo_breathalyser"
    ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Caml.Sys.chdir "using_ligo_breathalyser"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "test.mligo" ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}];
  run_ligo_good [ "run"; "test"; "test.mligo"; "--project-root"; "." ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  let test s =
    s
    |> String.split_lines
    |> List.length
    |> fun len -> if len > 0 then "Test passed" else "Test failed"
  in
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "import_import/main.mligo"
    ; "--project-root"
    ; "import_import"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "import_include/main.mligo"
    ; "--project-root"
    ; "import_include"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "include_import/main.mligo"
    ; "--project-root"
    ; "include_import"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "include_include/main.mligo"
    ; "--project-root"
    ; "include_include"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}]

(* ligo publish tests *)

let ligo_bin_path = "../../../../../install/default/bin/ligo"
let () = Caml.Sys.chdir "publish_invalid_main"

let%expect_test _ =
  run_ligo_bad [ "publish"; "--dry-run" ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: main file does not exists.
    Please specify a valid LIGO file in package.json. |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_invalid_main2"

let%expect_test _ =
  run_ligo_bad [ "publish"; "--dry-run" ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: Invalid LIGO file specifed in main field of package.json
    Valid extension for LIGO files are (.ligo, .mligo, .religo, .jsligo) |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_invalid_storage"

let%expect_test _ =
  run_ligo_bad [ "publish"; "--dry-run"; "--ligo-bin-path"; ligo_bin_path ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: Check `storage_fn` & `storage_arg` in packge.json or check your LIGO storage expression |}]

let () = Caml.Sys.chdir pwd

let clean_size ~prefix line =
  if String.is_prefix ~prefix line
  then
    if String.is_suffix ~suffix:"kB" line
    then Format.asprintf "%s*** kB" prefix
    else if String.is_suffix ~suffix:"MB" line
    then Format.asprintf "%s*** MB" prefix
    else if String.is_suffix ~suffix:"GB" line
    then Format.asprintf "%s*** GB" prefix
    else if String.is_suffix ~suffix:"B" line
    then Format.asprintf "%s*** B" prefix
    else line
  else line


let remove_dynamic_info_from_log log =
  String.split_lines log
  |> List.filter ~f:(fun line ->
         not
           (String.is_prefix ~prefix:"    shasum:" line
           || String.is_prefix ~prefix:"    integrity:" line))
  |> (fun lines ->
       List.map lines ~f:(fun line ->
           if String.is_prefix ~prefix:"    package size:  " line
           then clean_size ~prefix:"    package size:  " line
           else if String.is_prefix ~prefix:"    unpacked size: " line
           then clean_size ~prefix:"    unpacked size: " line
           else line))
  |> String.concat ~sep:"\n"


let () = Caml.Sys.chdir "publish_lib_lt_1mb"

let%expect_test _ =
  run_ligo_good [ "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_3@0.0.1
        === Tarball Details ===
        name:          test_package_3
        version:       0.0.1
        filename:      test_package_3-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** kB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_contract_lt_1mb"

let%expect_test _ =
  run_ligo_good [ "publish"; "--dry-run"; "--ligo-bin-path"; ligo_bin_path ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_4@0.0.1
        === Tarball Details ===
        name:          test_package_4
        version:       0.0.1
        filename:      test_package_4-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** kB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_contract_gt_1mb"

let%expect_test _ =
  run_ligo_good [ "publish"; "--dry-run"; "--ligo-bin-path"; ligo_bin_path ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_5@0.0.1
        === Tarball Details ===
        name:          test_package_5
        version:       0.0.1
        filename:      test_package_5-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** MB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "test_ligoignore"

let%expect_test _ =
  run_ligo_good [ "publish"; "--dry-run"; "--ligo-bin-path"; ligo_bin_path ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: testing_.ligoignore@0.0.1
        === Tarball Details ===
        name:          testing_.ligoignore
        version:       0.0.1
        filename:      testing_.ligoignore-0.0.1.tgz
        package size:  *** B
        unpacked size: *** B
        total files:   1 |}]

let () = Caml.Sys.chdir pwd
