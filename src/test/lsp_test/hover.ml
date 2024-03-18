open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type hover_test =
  { file : string
  ; hover_positions : Position.t list
        (* Each element of a list is a position where hover message should appear
           when mouse is on that position.
           This was introduced to make test output for multiple cases for one file more compact. *)
  }

let get_hover_test ({ file; hover_positions } : hover_test) : unit =
  let test_hover_for_position position =
    let path = normalize_path file in
    let actual_hover, diagnostics =
      test_run_session
      @@ let@ uri = open_file path in
         Requests.on_req_hover position uri
    in
    let test_info =
      Format.asprintf
        "Hover request for: %s, %a.\nDiagnostics for this test: %a"
        file
        Position.pp
        position
        Fmt.Dump.(list (pair Path.pp (list Diagnostic.pp)))
        (Path_hashtbl.to_alist diagnostics)
    in
    match actual_hover with
    | None -> failwith @@ "Expected a hover message, got none.\n" ^ test_info
    | Some hover ->
      Format.printf "%a" (Helpers_pretty.pp_with_yojson Hover.yojson_of_t) hover
  in
  run_multiple_tests hover_positions ~test_runner:test_hover_for_position


(* TODO after resolving some issues new hovers tests should be added:
   - #1959 add tests for typer error recovery (introduced in !2713)
   - #1676 add tests for hovers on constructors and record fields
   - #1965 add tests for e.g. `compose_endo` from `hovers.mligo`
*)
(* TODO JsLIGO tests *)

let pos = Position.create

let%expect_test "simple.mligo" =
  get_hover_test
    { file = "contracts/lsp/simple.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4
        ; pos ~line:0 ~character:5
        ; pos ~line:1 ~character:8
        ; pos ~line:1 ~character:9
        ; pos ~line:1 ~character:4
        ; pos ~line:1 ~character:5
        ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] }] |}]

let%expect_test "registry.jsligo" =
  get_hover_test
    { file = "contracts/lsp/registry.jsligo"
    ; hover_positions =
        [ pos ~line:11 ~character:19
        ; pos ~line:26 ~character:10
        ; pos ~line:28 ~character:31
        ; pos ~line:39 ~character:28
        ; pos ~line:40 ~character:50
        ; pos ~line:39 ~character:40
        ; pos ~line:39 ~character:55
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "get_exn : <a>(_: list<a>) => (_: int) => a",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "map :\n  <src, dst>(_: (_: src) => dst) => (_: list<src>) => list<\n    dst\n  >",
           "language": "jsligo"
         },
         "The call `map(f, list([a1; ...; an]))` applies the function `f` to\n    `a1`, ..., `an` (from left to right), and builds the list\n    `list([f(a1); ...; f(an)])` with the results returned by `f`."
       ]
     };
     { "contents": [ { "value": "primes : list<int>", "language": "jsligo" } ] };
     { "contents": [ { "value": "store : storage", "language": "jsligo" } ] };
     { "contents": [ { "value": "store : storage", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "type storage = list<int>", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "type return_ = [list<operation>, list<int>]",
           "language": "jsligo"
         }
       ]
     }] |}]

let%expect_test "hovers.mligo" =
  get_hover_test
    { file = "contracts/lsp/hovers.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:8
        ; pos ~line:4 ~character:61
        ; pos ~line:4 ~character:20
        ; pos ~line:5 ~character:12
        ; pos ~line:5 ~character:37
        ; pos ~line:5 ~character:47
        ; pos ~line:9 ~character:31
        ; pos ~line:9 ~character:70
        ; pos ~line:21 ~character:17
        ; pos ~line:40 ~character:18
        ; pos ~line:56 ~character:12
        ; pos ~line:58 ~character:28
        ; pos ~line:59 ~character:15
        ; pos ~line:65 ~character:41
        ; pos ~line:70 ~character:5
        ; pos ~line:72 ~character:23
        ; pos ~line:75 ~character:4
        ; pos ~line:75 ~character:35
        ; pos ~line:75 ~character:27
        ; pos ~line:77 ~character:28
        ; pos ~line:79 ~character:8
        ; pos ~line:83 ~character:20
        ; pos ~line:17 ~character:61
        ; pos ~line:11 ~character:15
        ; pos ~line:46 ~character:55
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "type 'a endo = Endo of ('a -> 'a)", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "type 'a endo = Endo of ('a -> 'a)", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "compose_endo_with_type_annotation :\n  'a.'a endo -> 'a endo -> 'a endo",
           "language": "cameligo"
         }
       ]
     }; { "contents": [ { "value": "f : a -> a", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : int -> int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "f1 : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : int -> int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : t", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : a -> a", "language": "cameligo" } ] };
     {
       "contents": [
         {
           "value": "map : 'src 'dst.('src -> 'dst) -> 'src list -> 'dst list",
           "language": "cameligo"
         },
         "The call `map f [a1; ...; an]` applies the function `f` to `a1`,\n    ..., `an` (from left to right), and builds the list\n    `[f a1; ...; f an]` with the results returned by `f`."
       ]
     };
     {
       "contents": [
         { "value": "type 'a list2 = 'a list list", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "x1 : int list list -> int list list2",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "endo_list2 : 'a.'a endo -> 'a list2 endo",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "z : key_hash option -> tez -> int -> (operation * address)",
           "language": "cameligo"
         }
       ]
     }; { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     {
       "contents": [
         {
           "value": "create_contract :\n  'param\n  'storage.('param, 'storage) entrypoint ->\n  key_hash option ->\n  tez -> 'storage -> (operation * address)",
           "language": "cameligo"
         },
         "The call `create_contract c e a s` returns a contract creation\n    operation (origination) for the entrypoint `e` (as a function)\n    with optional delegate `d`, initial amount `a` and initial\n    storage `s`, together with the address of the created\n    contract. Note that the created contract cannot be called\n    immediately afterwards (that is, `get_contract_opt` on that\n    address would return `None`), as the origination must be\n    performed successfully first, for example by calling a proxy\n    contract or itself."
       ]
     };
     {
       "contents": [
         {
           "value": "type 'v proxy_address =\n  ('v * nat * address, unit) typed_address",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "type 'v p = 'v Test.Proxy_ticket.proxy_address",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "type int_endo = IntEndo of (int -> int)",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "C.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/C.mligo"
    ; hover_positions =
        [ pos ~line:3 ~character:11
        ; pos ~line:3 ~character:13
        ; pos ~line:5 ~character:11
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "#import \"B.mligo\" \"M\"", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "#import \"A.mligo\" \"C\"", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "#import \"A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "outer.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/outer.mligo"
    ; hover_positions =
        [ pos ~line:2 ~character:12
        ; pos ~line:2 ~character:20
        ; pos ~line:2 ~character:23
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "#import \"inner/inner.mligo\" \"Inner\"",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "#import \"C.mligo\" \"Outer\"", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "#import \"A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "inner.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/inner/inner.mligo"
    ; hover_positions = [ pos ~line:2 ~character:12; pos ~line:2 ~character:17 ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "#import \"../C.mligo\" \"Outer\"", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "#import \"../A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "hover_module.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/hover_module.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:7
        ; pos ~line:10 ~character:7
        ; pos ~line:17 ~character:7
        ; pos ~line:33 ~character:24
        ; pos ~line:43 ~character:13
        ; pos ~line:48 ~character:12
        ; pos ~line:54 ~character:10
        ; pos ~line:70 ~character:20
        ; pos ~line:77 ~character:14
        ; pos ~line:25 ~character:9
        ; pos ~line:41 ~character:21
        ; pos ~line:28 ~character:11
        ; pos ~line:41 ~character:25
        ; pos ~line:35 ~character:8
        ; pos ~line:39 ~character:35
        ; pos ~line:62 ~character:9
        ; pos ~line:64 ~character:9
        ; pos ~line:5 ~character:12
        ; pos ~line:10 ~character:11
        ; pos ~line:48 ~character:26
        ; pos ~line:71 ~character:10
        ; pos ~line:66 ~character:12
        ; pos ~line:72 ~character:10
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "module A : sig\n  val foo : int\n\n  val bar : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module B : sig\n  type t =  nat\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module C : sig\n  val another : int\n\n  val foo : tez\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val length : bytes -> nat\n\n  val size : bytes -> nat\n\n  val concat : bytes -> bytes -> bytes\n\n  val concats : bytes list -> bytes\n\n  val sub : nat -> nat -> bytes -> bytes\n\n  val slice : nat -> nat -> bytes -> bytes\n\n  val pack : 'a.'a -> bytes\n\n  val unpack : 'a.bytes -> 'a option\n  end",
           "language": "cameligo"
         },
         "Sequences of bytes\n\n    Bytes are used for serializing data, in order to check signatures\n    and compute hashes on them. They can also be used to read untyped\n    data from outside of the contract."
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled : sig\n  val where : ^a\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled_with_sig : sig\n  type t\n\n  type int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled_with_inlined_sig : sig\n  val foo : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type With_included = sig\n  type t\n\n  type int =  string\n\n  val b : bool\n\n  val z : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module With_included : sig\n  type t =  int\n\n  type int =  string\n\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Outer : sig\n  val outer_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Outer : sig\n  val outer_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Inner : sig\n  val inner_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Inner : sig\n  val inner_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val overwritten : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val overwritten : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module M : sig\n  val v : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module M : sig\n  val v : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type I = sig\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type I = sig\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "hover_module.jsligo" =
  get_hover_test
    { file = "contracts/lsp/hover/hover_module.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:10
        ; pos ~line:10 ~character:10
        ; pos ~line:17 ~character:10
        ; pos ~line:33 ~character:27
        ; pos ~line:43 ~character:11
        ; pos ~line:48 ~character:13
        ; pos ~line:54 ~character:26
        ; pos ~line:72 ~character:17
        ; pos ~line:76 ~character:22
        ; pos ~line:25 ~character:11
        ; pos ~line:41 ~character:21
        ; pos ~line:28 ~character:20
        ; pos ~line:41 ~character:26
        ; pos ~line:35 ~character:13
        ; pos ~line:39 ~character:35
        ; pos ~line:62 ~character:12
        ; pos ~line:65 ~character:9
        ; pos ~line:5 ~character:10
        ; pos ~line:10 ~character:23
        ; pos ~line:48 ~character:38
        ; pos ~line:68 ~character:10
        ; pos ~line:72 ~character:35
        ; pos ~line:76 ~character:38
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "namespace A implements {\n  const foo: int;\n  const bar: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace B implements {\n  type t = nat;\n  type int = string;\n  const b: t\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace C implements {\n  const foo: tez;\n  const another: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const length: (_: bytes) => nat;\n  const size: (_: bytes) => nat;\n  const concat: (_: bytes) => (_: bytes) => bytes;\n  const concats: (_: list<bytes>) => bytes;\n  const sub: (_: nat) => (_: nat) => (_: bytes) => bytes;\n  const slice: (_: nat) => (_: nat) => (_: bytes) => bytes;\n  const pack: <a>(_: a) => bytes;\n  const unpack: <a>(_: bytes) => option<a>\n}",
           "language": "jsligo"
         },
         "Sequences of bytes\n\n    Bytes are used for serializing data, in order to check signatures\n    and compute hashes on them. They can also be used to read untyped\n    data from outside of the contract."
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled implements {\n  const where: ^a;\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled_with_sig implements {\n  const where: ^b;\n  type t = string;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled_with_inlined_sig implements {\n  const where: ^c;\n  const foo: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface With_included {\n  type t;\n  type int = string;\n  const b: bool;\n  const z: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace With_included implements {\n  type t = int;\n  type int = string;\n  const b: bool\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Outer implements {\n  const outer_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Outer implements {\n  const outer_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Inner implements {\n  const inner_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Inner implements {\n  const inner_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const overwritten: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const overwritten: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M implements {\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M implements {\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     }] |}]

let%expect_test "doc_comments.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/doc_comments.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:12
        ; pos ~line:15 ~character:11
        ; pos ~line:5 ~character:6
        ; pos ~line:8 ~character:7
        ; pos ~line:11 ~character:6
        ; pos ~line:15 ~character:7
        ; pos ~line:19 ~character:6
        ; pos ~line:22 ~character:7
        ; pos ~line:25 ~character:6
        ; pos ~line:29 ~character:8
        ; pos ~line:38 ~character:4
        ; pos ~line:40 ~character:8
        ; pos ~line:43 ~character:7
        ; pos ~line:49 ~character:6
        ; pos ~line:52 ~character:9
        ; pos ~line:54 ~character:8
        ; pos ~line:59 ~character:4
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "module type X = sig\n  [@view]\n  val y : int -> int\n\n  type t\n\n  val p : t option\n  end",
           "language": "cameligo"
         },
         "MODULE SIG"
       ]
     };
     {
       "contents": [
         {
           "value": "module type X = sig\n  [@view]\n  val y : int -> int\n\n  type t\n\n  val p : t option\n  end",
           "language": "cameligo"
         },
         "MODULE SIG"
       ]
     };
     {
       "contents": [
         { "value": "y : int -> int", "language": "cameligo" }, "SIG ITEM"
       ]
     };
     {
       "contents": [ { "value": "type t", "language": "cameligo" }, "SIG TYPE" ]
     };
     {
       "contents": [
         { "value": "p : t option", "language": "cameligo" }, "SIG ITEM"
       ]
     };
     {
       "contents": [
         {
           "value": "module M : sig\n  type t =  {foo : nat}\n\n  val p : t option\n\n  [@view]\n  val y : int -> int\n  end",
           "language": "cameligo"
         },
         "MODULE"
       ]
     };
     {
       "contents": [
         { "value": "y : int -> int", "language": "cameligo" }, "TERM IN MODULE"
       ]
     };
     {
       "contents": [
         { "value": "type t = {foo : nat}", "language": "cameligo" },
         "TYPE IN MODULE"
       ]
     };
     {
       "contents": [
         { "value": "p : M.t option", "language": "cameligo" }, "TERM IN MODULE"
       ]
     };
     {
       "contents": [
         { "value": "type 'a t = 'a list", "language": "cameligo" },
         "JUST A TYPE"
       ]
     };
     {
       "contents": [
         { "value": "x : int t", "language": "cameligo" },
         "JUST A TERM\n\n  with some doc\n  in **several** lines\n\n  one ~~more~~ `line`"
       ]
     };
     {
       "contents": [
         { "value": "x : int t", "language": "cameligo" },
         "JUST A TERM\n\n  with some doc\n  in **several** lines\n\n  one ~~more~~ `line`"
       ]
     };
     {
       "contents": [
         {
           "value": "module M1 : sig\n  [@entry]\n  val y : int -> int -> (operation list * int)\n  end",
           "language": "cameligo"
         },
         "MODULE WITH ENTRY POINT"
       ]
     };
     {
       "contents": [
         {
           "value": "y : int -> int -> (operation list * int)",
           "language": "cameligo"
         },
         "BEFORE DECORATOR",
         "AFTER DECORATOR",
         "ENTRY POINT TERM"
       ]
     };
     {
       "contents": [
         {
           "value": "module C : sig\n  val f : int -> int\n  end",
           "language": "cameligo"
         },
         "NESTED MODULE"
       ]
     };
     {
       "contents": [
         { "value": "f : int -> int", "language": "cameligo" },
         "NESTED MODULE TERM"
       ]
     };
     {
       "contents": [
         { "value": "t : int t", "language": "cameligo" },
         "Has type with comment inside"
       ]
     }] |}]

let%expect_test "doc_comments.jsligo" =
  get_hover_test
    { file = "contracts/lsp/hover/doc_comments.jsligo"
    ; hover_positions =
        [ pos ~line:1 ~character:10
        ; pos ~line:14 ~character:23
        ; pos ~line:5 ~character:8
        ; pos ~line:8 ~character:7
        ; pos ~line:10 ~character:8
        ; pos ~line:14 ~character:10
        ; pos ~line:18 ~character:15
        ; pos ~line:20 ~character:14
        ; pos ~line:22 ~character:15
        ; pos ~line:26 ~character:5
        ; pos ~line:35 ~character:6
        ; pos ~line:37 ~character:10
        ; pos ~line:40 ~character:11
        ; pos ~line:46 ~character:15
        ; pos ~line:49 ~character:19
        ; pos ~line:51 ~character:17
        ; pos ~line:56 ~character:6
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "interface X {\n  @view\n  const y: (_: int) => int;\n  type t;\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "INTERFACE"
       ]
     };
     {
       "contents": [
         {
           "value": "interface X {\n  @view\n  const y: (_: int) => int;\n  type t;\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "INTERFACE"
       ]
     };
     {
       "contents": [
         { "value": "y : (_: int) => int", "language": "jsligo" },
         "INTERFACE ITEM"
       ]
     };
     {
       "contents": [
         { "value": "type t", "language": "jsligo" }, "INTERFACE TYPE"
       ]
     };
     {
       "contents": [
         { "value": "p : option<t>", "language": "jsligo" }, "INTERFACE ITEM"
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M implements {\n  @view\n  const y: (x: int) => int;\n  type t = { foo: nat };\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "NAMESPACE"
       ]
     };
     {
       "contents": [
         { "value": "y : (x: int) => int", "language": "jsligo" },
         "TERM IN NAMESPACE"
       ]
     };
     {
       "contents": [
         { "value": "type t = { foo: nat }", "language": "jsligo" },
         "TYPE IN NAMESPACE"
       ]
     };
     {
       "contents": [
         { "value": "p : option<M.t>", "language": "jsligo" },
         "TERM IN NAMESPACE"
       ]
     };
     {
       "contents": [
         { "value": "type t<a> = list<a>", "language": "jsligo" }, "JUST A TYPE"
       ]
     };
     {
       "contents": [
         { "value": "x : t<int>", "language": "jsligo" },
         "JUST A TERM\nwith some doc\nin **several** lines\n\none ~~more~~ `line`"
       ]
     };
     {
       "contents": [
         { "value": "x : t<int>", "language": "jsligo" },
         "JUST A TERM\nwith some doc\nin **several** lines\n\none ~~more~~ `line`"
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M1 implements {\n  @entry\n  const y: (x: int, _: int) => [list<operation>, int]\n}",
           "language": "jsligo"
         },
         "NAMESPACE WITH ENTRY POINT"
       ]
     };
     {
       "contents": [
         {
           "value": "y : (x: int, _: int) => [list<operation>, int]",
           "language": "jsligo"
         },
         "BEFORE DECORATOR",
         "AFTER DECORATOR",
         "ENTRY POINT TERM"
       ]
     };
     {
       "contents": [
         {
           "value": "namespace C implements {\n  const f: (t: int) => int\n}",
           "language": "jsligo"
         },
         "NESTED NAMESPACE"
       ]
     };
     {
       "contents": [
         { "value": "f : (t: int) => int", "language": "jsligo" },
         "NESTED NAMESPACE TERM"
       ]
     };
     {
       "contents": [
         { "value": "t : (_: unit) => t<int>", "language": "jsligo" },
         "Has type with comment inside"
       ]
     }] |}]

let%expect_test "dynamic_entrypoints.mligo" =
  get_hover_test
    { file = "contracts/dynamic_entrypoints.mligo"
    ; hover_positions =
        [ pos ~line:7 ~character:5
        ; pos ~line:15 ~character:33
        ; pos ~line:32 ~character:30
        ; pos ~line:10 ~character:7
        ; pos ~line:23 ~character:35
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "tick : (int ticket, int * int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "tick : (int ticket, int * int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "dynamic_entrypoints.jsligo" =
  get_hover_test
    { file = "contracts/dynamic_entrypoints.jsligo"
    ; hover_positions =
        [ pos ~line:7 ~character:9
        ; pos ~line:15 ~character:35
        ; pos ~line:36 ~character:29
        ; pos ~line:11 ~character:9
        ; pos ~line:25 ~character:36
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "tick : dynamic_entrypoint<ticket<int>, [int, int]>",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "tick : dynamic_entrypoint<ticket<int>, [int, int]>",
           "language": "jsligo"
         }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the module type is unresolved. *)
let%expect_test "missing_module.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_module.mligo"
    ; hover_positions = [ pos ~line:0 ~character:7 ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "module A = (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the type annotation is unresolved. *)
let%expect_test "missing_type_annot.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_type_annot.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "a : (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the type is unresolved. *)
let%expect_test "missing_type.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_type.mligo"
    ; hover_positions = [ pos ~line:0 ~character:5 ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "type a = (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "Hovers for variables in complex patterns show the correct types" =
  get_hover_test
    { file = "contracts/get_scope_tests/complex_patterns.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4
        ; pos ~line:0 ~character:7
        ; pos ~line:3 ~character:6
        ; pos ~line:3 ~character:9
        ; pos ~line:6 ~character:9
        ; pos ~line:9 ~character:10
        ; pos ~line:9 ~character:13
        ; pos ~line:12 ~character:10
        ; pos ~line:12 ~character:13
        ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : string", "language": "cameligo" } ] };
     { "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : string", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "i : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "j : string", "language": "cameligo" } ] };
     { "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : string", "language": "cameligo" } ] }] |}]

let%expect_test "The original var preserves the module path" =
  get_hover_test
    { file = "contracts/lsp/hover/module_in_type.mligo"
    ; hover_positions =
        [ pos ~line:6 ~character:4
        ; pos ~line:7 ~character:4
        ; pos ~line:9 ~character:4
        ; pos ~line:10 ~character:7
        ; pos ~line:12 ~character:4
        ; pos ~line:13 ~character:7
        ; pos ~line:14 ~character:7
        ; pos ~line:20 ~character:4
        ; pos ~line:21 ~character:7
        ; pos ~line:22 ~character:7
        ; pos ~line:29 ~character:4
        ; pos ~line:30 ~character:7
        ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x1 : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y1 : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "x2 : M.u", "language": "cameligo" } ] };
     { "contents": [ { "value": "y2 : unit", "language": "cameligo" } ] };
     { "contents": [ { "value": "x3 : M.v", "language": "cameligo" } ] };
     { "contents": [ { "value": "y3 : M.u", "language": "cameligo" } ] };
     { "contents": [ { "value": "z3 : unit", "language": "cameligo" } ] };
     { "contents": [ { "value": "x4 : N.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y4 : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "z4 : unit", "language": "cameligo" } ] };
     { "contents": [ { "value": "x5 : O.u", "language": "cameligo" } ] };
     { "contents": [ { "value": "y5 : N.t", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path in failwith" =
  get_hover_test
    { file = "contracts/lsp/hover/failwith_module_path.mligo"
    ; hover_positions = [ pos ~line:6 ~character:4; pos ~line:7 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : A.B.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : A.B.t", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path inside option type" =
  get_hover_test
    { file = "contracts/lsp/hover/option_module_path.mligo"
    ; hover_positions = [ pos ~line:4 ~character:4 ]
    };
  [%expect
    {|
      [{ "contents": [ { "value": "x : A.t option", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path of an imported module" =
  get_hover_test
    { file = "contracts/lsp/hover/imported_module.mligo"
    ; hover_positions = [ pos ~line:2 ~character:4; pos ~line:3 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : M.t option", "language": "cameligo" } ] }] |}]

let%expect_test "Shows the correct path relative to the current env" =
  get_hover_test
    { file = "contracts/lsp/hover/module_access.mligo"
    ; hover_positions =
        [ pos ~line:3 ~character:6; pos ~line:6 ~character:4; pos ~line:6 ~character:10 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : M.t", "language": "cameligo" } ] }] |}]

let%expect_test "Shows the correct path relative to the current envs" =
  get_hover_test
    { file = "contracts/lsp/hover/module_accesses.mligo"
    ; hover_positions =
        [ pos ~line:14 ~character:10
        ; (* TODO: perhaps should yield the same type as above? *)
          pos ~line:14 ~character:4
        ; (* TODO: type M.export? *)
          pos ~line:16 ~character:11
        ; pos ~line:16 ~character:5
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "x : (M.u * O.t * O.u) option", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "y : (Import.M.u * N.t * N.u) option",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "type export = M.u * O.t * O.u", "language": "cameligo" }
       ]
     };
     { "contents": [ { "value": "type t = O.export", "language": "cameligo" } ] }] |}]

let%expect_test "Constructors and record fields hovers (CameLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/ctors_and_fields.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:5
        ; pos ~line:2 ~character:4
        ; pos ~line:3 ~character:4
        ; pos ~line:4 ~character:6
        ; pos ~line:4 ~character:13
        ; pos ~line:4 ~character:22
        ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "Foo", "language": "cameligo" } ] };
     { "contents": [ { "value": "Bar of int", "language": "cameligo" } ] };
     { "contents": [ { "value": "Baz of unit", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "Aaa of {\n a : int;\n b : bool\n}", "language": "cameligo" }
       ]
     }; { "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "cameligo" } ] }] |}]

let%expect_test "Constructors and record fields hovers (JsLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/ctors_and_fields.jsligo"
    ; hover_positions =
        [ pos ~line:1 ~character:7
        ; pos ~line:2 ~character:8
        ; pos ~line:3 ~character:8
        ; pos ~line:4 ~character:9
        ; pos ~line:4 ~character:14
        ; pos ~line:4 ~character:23
        ; pos ~line:5 ~character:8
        ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "[\"Foo\"]", "language": "jsligo" } ] };
     { "contents": [ { "value": "[\"Bar\", int]", "language": "jsligo" } ] };
     { "contents": [ { "value": "[\"Baz\", unit]", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "[\"Aaa\", { a: int; b: bool }]", "language": "jsligo" }
       ]
     }; { "contents": [ { "value": "a : int", "language": "jsligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] };
     { "contents": [ { "value": "[\"Bbb\", bool]", "language": "jsligo" } ] }] |}]

let%expect_test "Disc union fields" =
  get_hover_test
    { file = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:11
        ; pos ~line:0 ~character:38
        ; pos ~line:3 ~character:12
        ; pos ~line:0 ~character:24
        ; pos ~line:4 ~character:24
        ; pos ~line:0 ~character:52
        ; pos ~line:5 ~character:22
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "kind : [\"aaa\"] | [\"42\"]", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "kind : [\"aaa\"] | [\"42\"]", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "kind : [\"aaa\"] | [\"42\"]", "language": "jsligo" }
       ]
     }; { "contents": [ { "value": "a : int", "language": "jsligo" } ] };
     { "contents": [ { "value": "a : int", "language": "jsligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] }] |}]

let%expect_test "Polymorphic types (CameLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/poly_types.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:15
        ; pos ~line:2 ~character:5
        ; pos ~line:4 ~character:4
        ; pos ~line:4 ~character:21
        ; pos ~line:4 ~character:24
        ; pos ~line:4 ~character:28
        ; pos ~line:4 ~character:30
        ; pos ~line:6 ~character:4
        ; pos ~line:6 ~character:15
        ; pos ~line:6 ~character:17
        ; pos ~line:7 ~character:8
        ; pos ~line:8 ~character:9
        ; pos ~line:8 ~character:14
        ; pos ~line:8 ~character:16
        ; pos ~line:10 ~character:4
        ; pos ~line:10 ~character:33
        ; pos ~line:10 ~character:56
        ; pos ~line:16 ~character:4
        ; pos ~line:16 ~character:16
        ; pos ~line:18 ~character:4
        ; pos ~line:20 ~character:4
        ; pos ~line:22 ~character:4
        ; pos ~line:24 ~character:4
        ; pos ~line:30 ~character:4
        ; pos ~line:34 ~character:4
        ; pos ~line:38 ~character:4
        ; pos ~line:40 ~character:4
        ; pos ~line:47 ~character:10
        ; pos ~line:50 ~character:5
        ; pos ~line:52 ~character:4
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "type ('a, 'b) func = Func of ('a -> 'b)",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "incr : (int, int) func", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "apply_func : 'a 'b.('a, 'b) func -> 'a -> 'b",
           "language": "cameligo"
         }
       ]
     }; { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     {
       "contents": [
         {
           "value": "apply_func : 'a 'b.('a, 'b) func -> 'a -> 'b",
           "language": "cameligo"
         }
       ]
     };
     { "contents": [ { "value": "f : (a, b) func", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : (a, b) func", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };
     {
       "contents": [
         {
           "value": "apply_func :\n  'dom\n  'codom.('dom, 'codom) func -> 'dom -> 'codom",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "f : (dom, codom) func", "language": "cameligo" }
       ]
     }; { "contents": [ { "value": "x : dom", "language": "cameligo" } ] };
     {
       "contents": [ { "value": "f : 'a.'a t -> 'a t", "language": "cameligo" } ]
     }; { "contents": [ { "value": "r : a t", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "ticket : nat ticket option", "language": "cameligo" }
       ]
     };
     { "contents": [ { "value": "lst : int list", "language": "cameligo" } ] };
     { "contents": [ { "value": "opt : int option", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "big_map : (int, nat) t", "language": "cameligo" }
       ]
     };
     { "contents": [ { "value": "x : string M.t", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "foo : int Common.foo", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         { "value": "ok_result : (int, string) result", "language": "cameligo" }
       ]
     };
     {
       "contents": [
         {
           "value": "error_result : (int, string) result",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "type 'a b = B of 'a X.x", "language": "cameligo" }
       ]
     };
     { "contents": [ { "value": "type t = int list", "language": "cameligo" } ] };
     { "contents": [ { "value": "f : t -> int list", "language": "cameligo" } ] }] |}]

let%expect_test "Polymorphic types (JsLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/poly_types.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:5
        ; pos ~line:2 ~character:6
        ; pos ~line:4 ~character:9
        ; pos ~line:4 ~character:21
        ; pos ~line:4 ~character:24
        ; pos ~line:5 ~character:9
        ; pos ~line:6 ~character:15
        ; pos ~line:6 ~character:20
        ; pos ~line:6 ~character:22
        ; pos ~line:10 ~character:9
        ; pos ~line:10 ~character:34
        ; pos ~line:10 ~character:56
        ; pos ~line:18 ~character:9
        ; pos ~line:18 ~character:15
        ; pos ~line:22 ~character:6
        ; pos ~line:24 ~character:6
        ; pos ~line:26 ~character:6
        ; pos ~line:28 ~character:6
        ; pos ~line:34 ~character:6
        ; pos ~line:38 ~character:6
        ; pos ~line:40 ~character:6
        ; pos ~line:49 ~character:7
        ; pos ~line:52 ~character:5
        ; pos ~line:54 ~character:6
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "type func<a, b> = | [\"Func\", (x: a) => b]",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [ { "value": "incr : func<int, int>", "language": "jsligo" } ]
     };
     {
       "contents": [
         {
           "value": "apply_func : <a, b>(f: func<a, b>, x: a) => b",
           "language": "jsligo"
         }
       ]
     }; { "contents": [ { "value": "f : func<a, b>", "language": "jsligo" } ] };
     { "contents": [ { "value": "x : a", "language": "jsligo" } ] };
     { "contents": [ { "value": "f : func<a, b>", "language": "jsligo" } ] };
     { "contents": [ { "value": "f : (x: a) => b", "language": "jsligo" } ] };
     { "contents": [ { "value": "f : (x: a) => b", "language": "jsligo" } ] };
     { "contents": [ { "value": "x : a", "language": "jsligo" } ] };
     {
       "contents": [
         {
           "value": "apply_func2 :\n  <dom, codom>(f: func<dom, codom>, x: dom) => codom",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [ { "value": "f : func<dom, codom>", "language": "jsligo" } ]
     }; { "contents": [ { "value": "x : dom", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "f : <a>(r: t<a>) => t<a>", "language": "jsligo" }
       ]
     }; { "contents": [ { "value": "r : t<a>", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "ticket : option<ticket<nat>>", "language": "jsligo" }
       ]
     }; { "contents": [ { "value": "lst : list<int>", "language": "jsligo" } ] };
     { "contents": [ { "value": "opt : option<int>", "language": "jsligo" } ] };
     {
       "contents": [ { "value": "big_map : t<int, nat>", "language": "jsligo" } ]
     }; { "contents": [ { "value": "x : M.t<string>", "language": "jsligo" } ] };
     {
       "contents": [ { "value": "foo : Common.foo<int>", "language": "jsligo" } ]
     };
     {
       "contents": [
         {
           "value": "ok_result : Common.result<int, string>",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "type b<a> = | [\"B\", X.x<a>]", "language": "jsligo" }
       ]
     };
     { "contents": [ { "value": "type t = list<int>", "language": "jsligo" } ] };
     {
       "contents": [ { "value": "f3 : (x: t) => t<int>", "language": "jsligo" } ]
     }] |}]

let%expect_test "Recover from missing variable" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_variable.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4; pos ~line:1 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "g : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "h : int", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from missing module" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_module.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect {|
    [{ "contents": [ { "value": "a : int", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from missing record field" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_record_field.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect {|
    [{ "contents": [ { "value": "a : ^a", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_1.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:6; pos ~line:2 ~character:6; pos ~line:5 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error 2" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_2.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4; pos ~line:1 ~character:4; pos ~line:2 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "a : string", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "f : string -> string", "language": "cameligo" }
       ]
     }; { "contents": [ { "value": "g : string", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error 3" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_3.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4; pos ~line:1 ~character:4 ]
    };
  [%expect
    {|
    [{
       "contents": [ { "value": "g : 'a 'b.'a -> 'b", "language": "cameligo" } ]
     };
     {
       "contents": [ { "value": "h : 'a 'b.'a -> 'b", "language": "cameligo" } ]
     }] |}]
