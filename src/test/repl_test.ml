open Test_helpers
open Simple_utils.Trace
module Raw_options = Compiler_options.Raw_options

let dry_run_options =
  Lwt_main.run
    Proto_alpha_utils.Memory_proto_alpha.(
      Lwt.bind (test_environment ()) (fun env -> make_options ~env ()))


let raw_options = Raw_options.make ()
let options = Compiler_options.make ~raw_options ()

let make_init_state_cameligo ?(project_root = None) () =
  Repl.make_initial_state
    (CameLIGO : Syntax_types.t)
    Environment.Protocols.in_use
    dry_run_options
    project_root
    options


let init_state_cameligo = make_init_state_cameligo ()

let make_init_state_jsligo ?(project_root = None) () =
  Repl.make_initial_state
    (JsLIGO : Syntax_types.t)
    Environment.Protocols.in_use
    dry_run_options
    project_root
    options


let init_state_jsligo = make_init_state_jsligo ()

let apply_repl_sequence ~raw_options init_state commands =
  let f state command =
    let _, state, out =
      Repl.parse_and_eval ~raw_options (Ex_display_format Dev) state command
    in
    state, out
  in
  let _, trace = List.fold_map ~f ~init:init_state commands in
  trace


let test_seq ~raise ~raw_options init_state cmds res () =
  let r = apply_repl_sequence ~raw_options init_state cmds in
  if List.compare String.compare res r = 0 then () else raise.error @@ `Test_repl (res, r)


let test_basic ~raise ~raw_options () =
  let _, _, s =
    Repl.parse_and_eval ~raw_options (Ex_display_format Dev) init_state_cameligo "1 + 3"
  in
  if String.compare s "4" = 0 then () else raise.error @@ `Test_repl ([ s ], [ "4" ])


let test_stdlib ~raise ~raw_options () =
  let _, _, s =
    Repl.parse_and_eval
      ~raw_options
      (Ex_display_format Dev)
      init_state_cameligo
      "String.concat \"Hello \" \"world!\""
  in
  if String.compare s "\"Hello world!\"" = 0
  then ()
  else raise.error @@ `Test_repl ([ "\"Hello world!\"" ], [ s ])


let test_def ~raise () =
  test_seq ~raise init_state_cameligo [ "let f (x : int) = x * 2"; "f 3" ] [ "f"; "6" ] ()


let test_mod ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_cameligo
    [ "module EURO = struct\n\
       type t = int\n\
       let add (a , b : t * t) : t = a + b\n\
       let zero : t = 0\n\
       let one : t = 1\n\
       end"
    ; "EURO.one"
    ]
    [ "EURO"; "1" ]
    ()


let test_use ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_cameligo
    [ "#use \"contracts/build/A.mligo\""; "toto" ]
    [ "toto"; "1" ]
    ()


let test_long ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_cameligo
    [ "#use \"contracts/build/A.mligo\""
    ; "toto"
    ; "#import \"contracts/build/B.mligo\" \"MYMOD\""
    ; "MYMOD.toto"
    ; "MYMOD.A.toto"
    ; "let f (x : int) = MYMOD.main unit x"
    ; "f 4"
    ; "module EURO = struct\n\
       type t = nat\n\
       let add (a, b : t * t) : t = a + b\n\
       module CONST = struct\n\
       let zero : t = 0n\n\
       let one : t = 1n\n\
       end\n\
       end"
    ; "module US_DOLLAR = EURO"
    ; "US_DOLLAR.CONST.zero + 32n"
    ]
    [ "toto"
    ; "1"
    ; "Done."
    ; "32"
    ; "1"
    ; "f"
    ; "( LIST_EMPTY() , 48 )"
    ; "EURO"
    ; "US_DOLLAR"
    ; "+32"
    ]
    ()


let test_basic_jsligo ~raise ~raw_options () =
  let _, _, s =
    Repl.parse_and_eval ~raw_options (Ex_display_format Dev) init_state_jsligo "1 + 3"
  in
  if String.compare s "4" = 0 then () else raise.error @@ `Test_repl ([ s ], [ "4" ])


let test_stdlib_jsligo ~raise ~raw_options () =
  let _, _, s =
    Repl.parse_and_eval
      ~raw_options
      (Ex_display_format Dev)
      init_state_jsligo
      "String.concat(\"Hello \", \"world!\")"
  in
  if String.compare s "\"Hello world!\"" = 0
  then ()
  else raise.error @@ `Test_repl ([ "\"Hello world!\"" ], [ s ])


let test_def_jsligo ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_jsligo
    [ "let f = (x : int) : int => x * 2"; "f(3)" ]
    [ "f"; "6" ]
    ()


let test_mod_jsligo ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_jsligo
    [ "namespace EURO {\n\
       export type t = int;\n\
       export let add = ([a, b]: [t, t]): t => a + b;\n\
       export let zero: t = 0;\n\
       export let one: t = 1\n\
       }"
    ; "EURO.one"
    ]
    [ "EURO"; "1" ]
    ()


let test_use_jsligo ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_jsligo
    [ "#use \"contracts/build/A.mligo\""; "toto" ]
    [ "toto"; "1" ]
    ()


let test_long_jsligo ~raise ~raw_options () =
  test_seq
    ~raise
    ~raw_options
    init_state_jsligo
    [ "#use \"contracts/build/A.jsligo\""
    ; "toto"
    ; "#import \"contracts/build/B.jsligo\" \"MYMOD\""
    ; "MYMOD.toto"
    ; "MYMOD.A.toto"
    ; "let f = (x : int) : [list<operation>, int] => MYMOD.f (unit, x)"
    ; "f(4)"
    ; "namespace EURO {\n\
       export type t = nat;\n\
       export let add = ([a, b]: [t, t]): t => a + b;\n\
       export namespace CONST {\n\
       export let zero: t = 0 as nat;\n\
       export let one: t = 1 as nat;\n\
       };\n\
       }"
    ; "import US_DOLLAR = EURO"
    ; "US_DOLLAR.CONST.zero + (32 as nat)"
    ]
    [ "toto"
    ; "1"
    ; "Done."
    ; "32"
    ; "1"
    ; "f"
    ; "( LIST_EMPTY() , 48 )"
    ; "EURO"
    ; "US_DOLLAR"
    ; "+32"
    ]
    ()


let test_use_external_packages ~raise ~(raw_options : Raw_options.t) () =
  let project_root = Some "projects/demo" in
  let raw_options = { raw_options with project_root } in
  (* Here we #use (equivalent of #include) the dependencies of the root project *)
  test_seq
    ~raise
    ~raw_options
    (make_init_state_cameligo ~project_root ())
    [ "#use \"ligo-foo/foo.mligo\""
    ; "#use \"ligo-list-helpers/list.mligo\""
    ; "#use \"ligo-test_2/test2.mligo\""
    ; "y"
    ; "#use \"ligo_test_1/test1.mligo\""
    ; "x"
    ]
    [ "SetX , concat , reverse ,\nuniq_concat"
    ; "concat , reverse ,\nsum"
    ; "y"
    ; "24"
    ; "x"
    ; "42"
    ]
    ()


let test_import_external_packages ~raise ~(raw_options : Raw_options.t) () =
  let project_root = Some "projects/demo" in
  let raw_options = { raw_options with project_root } in
  (* Here we #import the dependecies of the root project under separate namespaces *)
  test_seq
    ~raise
    ~raw_options
    (make_init_state_cameligo ~project_root ())
    [ "#import \"ligo-foo/foo.mligo\" \"Foo\""
    ; "#import \"ligo-list-helpers/list.mligo\" \"ListX\""
    ; "#import \"ligo-test_2/test2.mligo\" \"Test2\""
    ; "#import \"ligo_test_1/test1.mligo\" \"Test1\""
    ; "Test1.x"
    ; "Test2.y"
    ]
    [ "Done."; "Done."; "Done."; "Done."; "42"; "24" ]
    ()


let test_use_scoped_package ~raise ~(raw_options : Raw_options.t) () =
  let project_root = Some "projects/using_scope_pkg_project" in
  let raw_options = { raw_options with project_root } in
  (* Here we #use (equivalent of #include) *)
  test_seq
    ~raise
    ~raw_options
    (make_init_state_cameligo ~project_root ())
    [ "#use \"@ligo/bigarray-cameligo/lib/bigarray.mligo\""; "reverse [3 ; 2 ; 1]" ]
    [ "big_array , construct , last , reverse , concat , find , set , insert ,\n\
       drop , take , slice , split , rotate , equal ,\n\
       remove"
    ; "CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY())))"
    ]
    ()


let test_import_scoped_packages ~raise ~(raw_options : Raw_options.t) () =
  let project_root = Some "projects/using_scope_pkg_project" in
  let raw_options = { raw_options with project_root } in
  test_seq
    ~raise
    ~raw_options
    (make_init_state_cameligo ~project_root ())
    [ "#import \"@ligo/bigarray-cameligo/lib/bigarray.mligo\" \"BA\""
    ; "BA.reverse [3 ; 2 ; 1]"
    ]
    [ "Done."; "CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY())))" ]
    ()


let () =
  Printexc.record_backtrace true;
  run_test
  @@ test_suite
       "LIGO"
       [ test_suite
           "REPL (cameligo)"
           [ test "basic" (test_basic ~raw_options)
           ; test "stdlib" (test_stdlib ~raw_options)
           ; test "def&eval" (test_def ~raw_options)
           ; test "mod" (test_mod ~raw_options)
           ; test "use" (test_use ~raw_options)
           ; test "long" (test_long ~raw_options)
           ]
       ; test_suite
           "REPL (jsligo)"
           [ test "basic" (test_basic_jsligo ~raw_options)
           ; test "stdlib" (test_stdlib_jsligo ~raw_options)
           ; test "def&eval" (test_def_jsligo ~raw_options)
           ; test "mod" (test_mod_jsligo ~raw_options)
           ; test "use" (test_use_jsligo ~raw_options)
           ; test "long" (test_long_jsligo ~raw_options)
           ]
       ; test_suite
           "REPL + package-management"
           [ test "#use ext pkgs" (test_use_external_packages ~raw_options)
           ; test "#import ext pkgs" (test_import_external_packages ~raw_options)
           ; test "#use scoped ext pkg" (test_use_scoped_package ~raw_options)
           ; test "#import scoped ext pkg" (test_import_scoped_packages ~raw_options)
           ]
       ];
  ()
